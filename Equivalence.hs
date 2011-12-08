{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, BangPatterns, NoMonomorphismRestriction, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, NamedFieldPuns, FlexibleContexts, TypeFamilies, OverlappingInstances, CPP #-} 
{-# OPTIONS -Wall -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-type-defaults #-} -- disabled warnings are because Printf-TH triggers them

-- | Utility functions for Patrick Bahr's /equivalence/ package
module Equivalence(
    -- * Typeclasses
    IsEquivalenceClass(..),HasEquivalence(..), eqvRep, eqvEquivalents,
    -- * Equivalence classes
    EquivalenceClass, ec_elements, ec_map, ec_mapMonotonic, 
    ec_singleton, ec_union, ec_join, ec_elementList,
    -- * Equivalence relations
    Equivalence, eqv_classmap, eqv_classes, eqv_eq, mkEquivalence, eqv_classcount,
    eqv_class_elements, eqv_reps, eqv_generators, eqv_equivalents, eqv_rep,
    eqv_classOf,
    -- * Testing
    qc_Equivalence, mkWellDefinednessProp, mkWellDefinednessProp2)   where

import Collections as Set
import Data.Equivalence.Monad
import Data.Function
import Data.Hashable
import Data.List(foldl')
import Data.Monoid
import HomogenousTuples
import Test.QuickCheck
import Test.QuickCheck.All
import Text.Printf.TH
import PrettyUtil
import QuickCheckUtil
import Element


#define KEYCLASS(A) Ord(A)

class IsEquivalenceClass cls elt | cls -> elt where
    canonicalRep :: cls -> elt
    equivalents :: cls -> [elt] 
    ecMember :: elt -> cls -> Bool
    ecSize :: cls -> Int

class IsEquivalenceClass cls elt => HasEquivalence t cls elt | cls -> elt, elt t -> cls where
    eqvClasses :: t -> [cls]
    -- | Throws an error if the element is not in the domain of the equivalence
    eqvClassOf :: t -> elt -> cls

eqvRep ::  HasEquivalence t cls elt => t -> elt -> elt
eqvRep e x = canonicalRep $! eqvClassOf e x 

eqvEquivalents ::  HasEquivalence t cls elt => t -> elt -> [elt]
eqvEquivalents e x = equivalents $! eqvClassOf e x

data EquivalenceClass a = EquivalenceClass {
    -- | INVARIANT: This contains 'ec_rep'
    ec_elements :: Set a,
    -- | An arbitraily chosen Representative of the equivalence class 
    ec_rep :: !a
}

ec_elementList :: EquivalenceClass a -> [a]
ec_elementList = setToList . ec_elements 

instance KEYCLASS(a) => IsEquivalenceClass (EquivalenceClass a) a where
    equivalents = setToList . ec_elements

    canonicalRep = ec_rep 

    ecMember x ec = x `member` ec_elements ec

    ecSize = setSize . ec_elements




ec_map :: (KEYCLASS(a), KEYCLASS(b)) => (a -> b) -> EquivalenceClass a -> EquivalenceClass b
ec_map f EquivalenceClass{ec_elements,ec_rep} = EquivalenceClass{ec_elements= mapSet f ec_elements, ec_rep=f ec_rep}

ec_mapMonotonic :: (KEYCLASS(a), KEYCLASS(b)) => (a -> b) -> EquivalenceClass a -> EquivalenceClass b
ec_mapMonotonic f EquivalenceClass{ec_elements,ec_rep} = 
    EquivalenceClass{ec_elements= mapSetMonotonic f ec_elements, ec_rep=f ec_rep}

deriving instance (Show a, KEYCLASS(a)) => Show (EquivalenceClass a)

instance Eq a => Eq (EquivalenceClass a) where
    (==) = (==) `on` ec_rep

instance Ord a => Ord (EquivalenceClass a) where
    compare = compare `on` ec_rep



ec_singleton :: KEYCLASS(a) => a -> EquivalenceClass a
ec_singleton a = EquivalenceClass (singletonSet a) a

ec_union :: KEYCLASS(a) => EquivalenceClass a -> EquivalenceClass a -> EquivalenceClass a
ec_union (EquivalenceClass es1 r1) (EquivalenceClass es2 _) = EquivalenceClass (es1 `mappend` es2) r1

data Equivalence a = Equivalence {
    eqv_classmap :: Map a (EquivalenceClass a),
    -- | Distinct.
    eqv_classes :: [EquivalenceClass a],
    eqv_generators :: [(a,a)]
}

deriving instance (Show a, KEYCLASS(a)) => Show (Equivalence a)

instance KEYCLASS(a) => HasEquivalence (Equivalence a) (EquivalenceClass a) a where
    eqvClasses = eqv_classes
    eqvClassOf = eqv_classOf
    
eqv_classOf :: Ord k => Equivalence k -> k -> EquivalenceClass k
eqv_classOf e x = eqv_classmap e ! x


-- | Checks whether the given elements are equivalent. Throws an error if the first argument is not in the domain of the equivalence.
eqv_eq :: (KEYCLASS(a)) => Equivalence a -> a -> a -> Bool
eqv_eq e x y = x `ecMember` eqvClassOf e y 

eqv_classcount ::  Equivalence a -> Int
eqv_classcount = length . eqv_classes


-- | Gets the representative of the equivalence class of the given element. Throws an error if the element is not in the domain of the equivalence.
eqv_rep :: (KEYCLASS(a)) => Equivalence a -> a -> a
eqv_rep !e !x = ec_rep (eqv_classOf e x)


-- insertUnlessExists = Map.insertWith (curry snd)


mkEquivalence :: forall a. (KEYCLASS(a)) => 
    [(a,a)]  -- ^ Gluings
    -> [a]   -- ^ Additional elements that should be present in the returned relation (will only be equivalent to themselves
             -- if they don't occur in the first argument)
    -> Equivalence a
mkEquivalence pairs extraElements = mkEquivalence0 (pairs ++ [(a,a) | a <- extraElements])

 
mkEquivalence0 :: forall a. (KEYCLASS(a)) => [(a,a)] -> Equivalence a
mkEquivalence0 pairs = (runEquivM ec_singleton ec_union go)
                            -- this is semantically redundant but makes it possible to retrieve eqv_generators without
                            -- computing the other stuff
                            { eqv_generators = pairs }
                        
    where
        go :: forall s. EquivM s (EquivalenceClass a) a (Equivalence a)
        go = do
            mapM_ (uncurry equate) pairs

            -- Now capture the information contained in the state of the EquivM monad into our pure
            -- Equivalence structure
            
            let f :: EquivM s (EquivalenceClass a) a (Equivalence a) 
                  -> a 
                  -> EquivM s (EquivalenceClass a) a (Equivalence a)
                f mr a = do

                    -- Get the recursive result
                    r@Equivalence { 
                        eqv_classmap = cm0, 
                        eqv_classes = cs0} <- mr

                    -- Get the equivalence class of the current element
                    cls <- classDesc a


                    return $ if a `memberOfMap` cm0
                                -- if the current element is already in the result Equivalence, do nothing
                                then r

                                else r { 
                                        eqv_classmap = mapInsert a cls cm0
                                      , eqv_classes = 
                                            -- Add the equivalence class to the eqv_classes list
                                            -- only if the current element is the representative of the
                                            -- equivalence class (to get a distinct list)
                                            if a == ec_rep cls
                                               then cls : cs0
                                               else cs0
                                     }


            foldl' f (return (Equivalence emptyMap [] pairs)) allElems
            

        allElems = catPairs pairs


catPairs ::  [(b, b)] -> [b]
catPairs = concatMap (\(a1,a2) -> [a1,a2])


mkProp :: Testable prop => ([Int] -> [(Int, Int)] -> Equivalence Int -> prop) -> Property
mkProp p = forAll gen (\(pairs :: [(Int,Int)]) ->
    let
        allElems = setToList . setFromList $ catPairs pairs
        n = length allElems
        e = mkEquivalence0 pairs
        classes = eqv_classes e
        nclasses = length classes

    in
        label ($(printf "%3d classes") (length classes)) $
        label ($(printf "%2.1f elements/class") (fromIntegral n / fromIntegral nclasses)) $
            (p allElems pairs e))
  where
    gen = sized (\n -> vectorOf (1+(n`div`5)) (resize (n `div` 4) arbitrary))

prop_refl ::  Property
prop_refl = mkProp (\elems _ e -> 
                forAll (elements elems) (\x -> eqv_eq e x x)) 
prop_sym ::  Property
prop_sym  = mkProp (\elems _ e -> 
                forAll2 (elements elems) (elements elems) (\x y ->
                        classify (x/=y) ("x /= y") $
                        eqv_eq e x y == eqv_eq e y x))
prop_trans ::  Property
prop_trans = mkProp (\elems _ e -> 
                forAll (elements elems) (\x ->
                    forAll (elements (Prelude.filter (\y -> eqv_eq e x y) elems)) (\y ->
                        forAll (elements (Prelude.filter (\z -> eqv_eq e y z) elems)) (\z ->
                            eqv_eq e x z))))


-- | All input pairs are equivalent in the result
prop_hull ::  Property
prop_hull = mkProp (\_ pairs e -> 
                forAll (elements pairs) (uncurry (eqv_eq e))) 

-- | A representative is contained in its own class
prop_rep ::  Property
prop_rep = mkProp (\_ _ e -> 
                forAll (elements (eqv_classes e)) (\ec -> ec_rep ec `ecMember` ec))

-- | The value set of 'eqv_classmap' equals 'eqv_classes'
prop_classmap_classes ::  Property
prop_classmap_classes = mkProp (\_ _ e -> naiveECSetEq 
                                            (elems (eqv_classmap e))
                                            (eqv_classes e))
    where
        naiveECSetEq = (==) `on`  (setFromList . fmap (\(EquivalenceClass es1 r1) -> (es1,r1))) 

-- | The key set of 'eqv_classmap' equals the set of elements occuring in the input pairs
prop_domain ::  Property
prop_domain = mkProp (\es _ e -> ((==) `on` setFromList) es (keys (eqv_classmap e))) 

-- | The union of the 'eqv_classes' equals the set of elements occuring in the input pairs
prop_classes_union ::  Property
prop_classes_union = mkProp (\es _ e -> ((==) `on` setFromList)
                                es
                                (concatMap (setToList . ec_elements) (eqv_classes e)))

-- | The equivalence test as implemented is the same as testing equality of representatives
prop_eq_by_reps ::  Property
prop_eq_by_reps = mkProp (\es _ e -> 
                forAll2 (elements es) (elements es) (\x y ->
                    eqv_eq e x y == (((==) `on` (eqv_rep e)) x y))) 

prop_classes_distinct ::  Property
prop_classes_distinct = mkProp (\_ _ e -> let cs = eqv_classes e in setSize (setFromList cs) == length cs)
                            

qc_Equivalence ::  IO Bool
qc_Equivalence = $(quickCheckAll)

instance (KEYCLASS(a), Pretty a) => Pretty (EquivalenceClass a) where pretty = pretty . ec_elements
instance (KEYCLASS(a), Pretty a) => Pretty (Equivalence a) where 
    pretty e =
        vsep 
            (text "Equivalence, classes = {"
             : fmap (indent 2 . pretty) (eqv_classes e) 
             ++ [rbrace])

ec_join :: KEYCLASS(a) => EquivalenceClass (EquivalenceClass a) -> EquivalenceClass a
ec_join ecs = EquivalenceClass { ec_elements = (foldr1 setUnion . fmap ec_elements . setToList . ec_elements) ecs
                               , ec_rep = (ec_rep . ec_rep) ecs
                               }

eqv_map :: (KEYCLASS(a), KEYCLASS(b)) => (a -> b) -> Equivalence a -> Equivalence b
eqv_map f e = mkEquivalence0 (fmap (map2 f) (eqv_generators e))

mkWellDefinednessProp :: (Eq y, Eq a, Show a, Show y) => (a -> [a]) -> (a -> y) -> a -> Property
mkWellDefinednessProp getEquivalents f x = 
        printTestCase (unlines ["=== FIRST EQUIVALENCE CLASS ELEMENT ==="
                               ,show x
                               ,"=== FUNCTION VALUE AT FIRST ELEMENT"
                               ,show fx ])

        (conjoin (fmap sameValue (getEquivalents x)))
    where                                           
        !fx = f x
        sameValue x' = classify (x==x') ("trivial (same representative)") $ 
                        printTestCase (unlines 
                               ["=== SECOND EQUIVALENCE CLASS ELEMENT ==="
                               ,show x'
                               ,"=== FUNCTION VALUE AT SECOND ELEMENT"
                               ,show fx' ]) $
                    
                            (fx' == fx)
            where
                fx' = f x'

mkWellDefinednessProp2 :: (Eq t1, Eq t, Eq y, Show t, Show t1, Show y) =>(t -> [t]) -> (t1 -> [t1]) -> (t -> t1 -> y) -> (t, t1) -> Property
mkWellDefinednessProp2 equivalentXs equivalentYs f =
        mkWellDefinednessProp 
            (\(x,y) -> equivalentXs x `cart` equivalentYs y)
            (uncurry f)




instance Hashable a => Hashable (EquivalenceClass a) where hash = hash . ec_rep

eqv_class_elements :: (KEYCLASS(a)) => Equivalence a -> a -> Set a
eqv_class_elements e = ec_elements . eqvClassOf e

eqv_equivalents :: (KEYCLASS(a)) => Equivalence a -> a -> [a]
eqv_equivalents e = equivalents . eqvClassOf e

-- | Returns a list containing a represenative of each class 
eqv_reps ::  Equivalence b -> [b]
eqv_reps e = ec_rep `fmap` eqv_classes e 


type instance Element (EquivalenceClass a) = a

instance AsList (EquivalenceClass a) where
    asList = ec_elementList

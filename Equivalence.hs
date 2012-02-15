{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, BangPatterns, NoMonomorphismRestriction, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, NamedFieldPuns, FlexibleContexts, TypeFamilies, OverlappingInstances, CPP #-} 
{-# OPTIONS -Wall -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-type-defaults #-} -- disabled warnings are because Printf-TH triggers them

-- | Utility functions for Patrick Bahr's /equivalence/ package
module Equivalence(
    module Equivalence.Class,
    -- * Equivalence classes
    SetBasedEquivalenceClass, ec_elements, ec_map, ec_mapMonotonic, 
    ec_singleton, ec_union, ec_join, ec_elementList, ec_rep,
    -- * Equivalence relations
    Equivalence, eqv_classmap, eqv_classes, eqv_eq, mkEquivalence, mkEquivalence0, eqv_classcount,
    eqv_class_elements, eqv_reps, eqv_generators, eqv_equivalents, eqv_rep,
    eqv_classOf, eqv_classOf_safe
    )   where

import EitherC
import Data.Equivalence.Monad
import Data.Function
import Data.Hashable
import Data.List(foldl')
import Data.Monoid
import HomogenousTuples
import PrettyUtil
import Element
import Equivalence.Class
import Util
import Control.DeepSeq.TH
import OrphanInstances() -- NFData Set, NFData Map
import qualified Data.Set as S
import Data.Set(Set)
import qualified Data.Map as M
import Data.Map(Map)



data SetBasedEquivalenceClass a = SetBasedEquivalenceClass {
    -- | INVARIANT: This contains 'ec_rep'
    ec_elements :: Set a,
    -- | An arbitraily chosen Representative of the equivalence class 
    ec_rep :: !a
}

ec_elementList :: SetBasedEquivalenceClass a -> [a]
ec_elementList = S.toList . ec_elements 

type instance Element (SetBasedEquivalenceClass a) = a

instance AsList (SetBasedEquivalenceClass a) where
    asList = ec_elementList

instance Ord a => IsEquivalenceClass (SetBasedEquivalenceClass a) where
    canonicalRep = ec_rep 

    ecSize = S.size . ec_elements




ec_map :: (Ord a, Ord b) => (a -> b) -> SetBasedEquivalenceClass a -> SetBasedEquivalenceClass b
ec_map f SetBasedEquivalenceClass {ec_elements,ec_rep} = 
    SetBasedEquivalenceClass {ec_elements= S.map f ec_elements, ec_rep=f ec_rep}

ec_mapMonotonic :: (Ord a, Ord b) => (a -> b) -> SetBasedEquivalenceClass a -> SetBasedEquivalenceClass b
ec_mapMonotonic f SetBasedEquivalenceClass {ec_elements,ec_rep} = 
    SetBasedEquivalenceClass {ec_elements= S.mapMonotonic f ec_elements, ec_rep=f ec_rep}

deriving instance (Show a, Ord a) => Show (SetBasedEquivalenceClass a)

instance Eq a => Eq (SetBasedEquivalenceClass a) where
    (==) = (==) `on` ec_rep

instance Ord a => Ord (SetBasedEquivalenceClass a) where
    compare = compare `on` ec_rep



ec_singleton :: Ord a => a -> SetBasedEquivalenceClass a
ec_singleton a = SetBasedEquivalenceClass (S.singleton a) a

ec_union :: Ord a => SetBasedEquivalenceClass a -> SetBasedEquivalenceClass a -> SetBasedEquivalenceClass a
ec_union (SetBasedEquivalenceClass es1 r1) (SetBasedEquivalenceClass es2 _) = SetBasedEquivalenceClass (es1 `mappend` es2) r1

data Equivalence a = Equivalence {
    eqv_classmap :: Map a (SetBasedEquivalenceClass a),
    -- | Distinct.
    eqv_classes :: [SetBasedEquivalenceClass a],
    eqv_generators :: [(a,a)]
}

type instance EquivalenceClassOf (Equivalence a) = SetBasedEquivalenceClass a
type instance Element (Equivalence a) = a

deriving instance (Show a, Ord a) => Show (Equivalence a)

instance Ord a => IsEquivalence (Equivalence a) where
    eqvClassOf_safe = eqv_classOf_safe

instance Ord a => EnumerableEquivalence (Equivalence a) where
    eqvClasses = eqv_classes
    
eqv_classOf :: Ord k => Equivalence k -> k -> SetBasedEquivalenceClass k
eqv_classOf = eqvClassOf

eqv_classOf_safe
  :: Ord k =>
     Equivalence k -> k -> AttemptC (SetBasedEquivalenceClass k)
eqv_classOf_safe e x = 
    case M.lookup x (eqv_classmap e) of
         Just y -> return y
         Nothing -> toAttemptC $ ($failureStr "eqv_classOf_safe: Element not in the equivalence relation") 


-- | Checks whether the given elements are equivalent. Throws an error if the first argument is not in the domain of the equivalence.
eqv_eq :: (Ord a) => Equivalence a -> a -> a -> Bool
eqv_eq = eqvEquivalent

eqv_classcount ::  Equivalence a -> Int
eqv_classcount = length . eqv_classes


-- | Gets the representative of the equivalence class of the given element. Throws an error if the element is not in the domain of the equivalence.
eqv_rep :: (Ord a) => Equivalence a -> a -> a
eqv_rep !e !x = ec_rep (eqv_classOf e x)


-- insertUnlessExists = Map.insertWith (curry snd)


mkEquivalence :: forall a. (Ord a) => 
    [(a,a)]  -- ^ Gluings
    -> [a]   -- ^ Additional elements that should be present in the returned relation (will only be equivalent to themselves
             -- if they don't occur in the first argument)
    -> Equivalence a
mkEquivalence pairs extraElements = mkEquivalence0 (pairs ++ [(a,a) | a <- extraElements])

 
-- | Like 'mkEquivalence', but without additional elements
mkEquivalence0 :: forall a. (Ord a) => [(a,a)] -> Equivalence a
mkEquivalence0 pairs = (runEquivM ec_singleton ec_union go)
                            -- this is semantically redundant but makes it possible to retrieve eqv_generators without
                            -- computing the other stuff
                            { eqv_generators = pairs }
                        
    where
        go :: forall s. EquivM s (SetBasedEquivalenceClass a) a (Equivalence a)
        go = do
            mapM_ (uncurry equate) pairs

            -- Now capture the information contained in the state of the EquivM monad into our pure
            -- Equivalence structure
            
            let f :: EquivM s (SetBasedEquivalenceClass a) a (Equivalence a) 
                  -> a 
                  -> EquivM s (SetBasedEquivalenceClass a) a (Equivalence a)
                f mr a = do

                    -- Get the recursive result
                    r@Equivalence { 
                        eqv_classmap = cm0, 
                        eqv_classes = cs0} <- mr

                    -- Get the equivalence class of the current element
                    cls <- classDesc a


                    return $ if a `M.member` cm0
                                -- if the current element is already in the result Equivalence, do nothing
                                then r

                                else r { 
                                        eqv_classmap = M.insert a cls cm0
                                      , eqv_classes = 
                                            -- Add the equivalence class to the eqv_classes list
                                            -- only if the current element is the representative of the
                                            -- equivalence class (to get a distinct list)
                                            if a == ec_rep cls
                                               then cls : cs0
                                               else cs0
                                     }


            foldl' f (return (Equivalence M.empty [] pairs)) allElems
            

        allElems = catPairs pairs






instance (Ord a, Pretty a) => Pretty (SetBasedEquivalenceClass a) where 
    pretty = prettyClass

instance (Ord a, Pretty a) => Pretty (Equivalence a) where 
    pretty = prettyEquivalence

ec_join :: Ord a => SetBasedEquivalenceClass (SetBasedEquivalenceClass a) -> SetBasedEquivalenceClass a
ec_join ecs = SetBasedEquivalenceClass { ec_elements = (foldr1 S.union . fmap ec_elements . S.toList . ec_elements) ecs
                               , ec_rep = (ec_rep . ec_rep) ecs
                               }

eqv_map :: (Ord a, Ord b) => (a -> b) -> Equivalence a -> Equivalence b
eqv_map f e = mkEquivalence0 (fmap (map2 f) (eqv_generators e))





instance Hashable a => Hashable (SetBasedEquivalenceClass a) where hash = hash . ec_rep

eqv_class_elements :: (Ord a) => Equivalence a -> a -> Set a
eqv_class_elements e = ec_elements . eqvClassOf e

eqv_equivalents :: (Ord a) => Equivalence a -> a -> [a]
eqv_equivalents e = asList . eqvClassOf e

-- | Returns a list containing a represenative of each class 
eqv_reps ::  Equivalence b -> [b]
eqv_reps e = ec_rep `fmap` eqv_classes e 


deriveNFData ''Equivalence
deriveNFData ''SetBasedEquivalenceClass

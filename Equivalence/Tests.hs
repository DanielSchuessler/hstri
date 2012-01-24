 {-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, BangPatterns, NoMonomorphismRestriction, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, NamedFieldPuns, FlexibleContexts, TypeFamilies, OverlappingInstances, CPP #-} 
{-# OPTIONS -Wall -fno-warn-unused-binds -fno-warn-name-shadowing -fno-warn-type-defaults #-} -- disabled warnings are because Printf-TH triggers them

module Equivalence.Tests where

import Collections as Set
import Data.Function
import Test.QuickCheck
import Test.QuickCheck.All
import QuickCheckUtil
import Equivalence
import Util
import Control.Arrow

mkProp :: Testable prop => ([Int] -> [(Int, Int)] -> Equivalence Int -> prop) -> Property
mkProp p = forAll gen (\(pairs :: [(Int,Int)]) ->
    let
        allElems = setToList . setFromList $ catPairs pairs
        n = length allElems
        e = mkEquivalence0 pairs
        classes = eqv_classes e
        nclasses = length classes

    in
--         label ($(printf "%3d classes") (length classes)) $
--         label ($(printf "%2.1f elements/class") (fromIntegral n / fromIntegral nclasses)) $
            (p allElems pairs e))
  where
    gen = sized (\n -> vectorOf (1+(n`div`5)) (resize (n `div` 4) arbitrary))

mkWellDefinednessProp :: (Eq y, Eq a, Show a, Show y) => (a -> [a]) -> (a -> y) -> a -> Property
mkWellDefinednessProp getEquivalents f x = 
        printTestCase (unlines ["=== FIRST EQUIVALENCE CLASS ELEMENT ==="
                               ,show x
                               ,"=== FUNCTION VALUE AT FIRST ELEMENT"
                               ,show fx ])

        (conjoin' (fmap sameValue (getEquivalents x)))
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
                forAll (elements (eqv_classes e)) (\ec -> ec_rep ec `Set.member` ec_elements ec))

-- | The value set of 'eqv_classmap' equals 'eqv_classes'
prop_classmap_classes ::  Property
prop_classmap_classes = mkProp (\_ _ e -> naiveECSetEq 
                                            (elems (eqv_classmap e))
                                            (eqv_classes e))
    where
        naiveECSetEq = (==) `on`  (setFromList . fmap (ec_elements &&& ec_rep))

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


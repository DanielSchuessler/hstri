{-# LANGUAGE TypeFamilies, TemplateHaskell, FlexibleContexts, ViewPatterns, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module QuickCheckUtil where

import Test.QuickCheck
import Control.Monad
import qualified Data.Set as S
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph
import qualified Data.Vector as V
import Element
import qualified Data.Vector.Generic as VG
import Data.SumType
import Control.Applicative
import Data.Maybe
-- import Math.NumberTheory.Primes.Factorisation

cart ::  Monad m => m a1 -> m a2 -> m (a1, a2)
cart = liftM2 (,)

forAllElements
  :: (Show (Element xs), AsList xs, Testable prop) =>
     xs -> (Element xs -> prop) -> Property
forAllElements (asList -> xs) p = 
    if null xs
       then label "Vacuously true (empty domain)" True
       else forAll (elements xs) p

-- | Shrinks the index in the given list
forAllElementsShrink
  :: (Show (Element xs), AsList xs, Testable prop) =>
     xs -> (Element xs -> prop) -> Property
forAllElementsShrink (asList -> xs) p = 
    case length xs of
        0 -> label "Vacuously true (empty domain)" True
        n -> forAllShrink (choose (0,n-1)) shrink (\i -> let x = xs !! i in
                                                             printTestCase (show x) (p x))

forAllElements2
  :: (Show (Element xs),
      Show (Element xs1),
      AsList xs,
      AsList xs1,
      Testable prop) =>
     xs -> xs1 -> ((Element xs, Element xs1) -> prop) -> Property
forAllElements2 xs ys p = forAllElements (asList xs `cart` asList ys) p


forAll2 :: (Testable prop, Show a1, Show a) =>Gen a -> Gen a1 -> (a -> a1 -> prop) -> Property
forAll2 gen1 gen2 p = forAll gen1 (\x1 -> forAll gen2 (p x1))
forAll3 :: (Testable prop, Show a1, Show a2, Show a) =>Gen a -> Gen a1 -> Gen a2 -> (a -> a1 -> a2 -> prop) -> Property
forAll3 gen1 gen2 gen3 p = forAll gen1 (\x1 -> forAll gen2 (\x2 -> forAll gen3 (p x1 x2)))

changeSize ::  (Int -> Int) -> Gen a -> Gen a
changeSize f gen = sized (\n -> resize (f n) gen) 

-- | Exhaustively checks a property for all elements of a list (in contrast to 'forAll', which samples randomly)
conjoinMap ::  Testable prop => [t] -> (t -> prop) -> Property
conjoinMap [] _ = label "Vacuously true (empty domain)" True
conjoinMap xs p = conjoin (fmap p xs)

conjoinMap2 :: Testable prop => [a1] -> [a2] -> ((a1, a2) -> prop) -> Property
conjoinMap2 xs ys p = conjoinMap (xs `cart` ys) p




(.=.) ::  (Show a, Eq a) => a -> a -> Property
x .=. y = 
    printTestCase (unlines ["Checking equality","=== LHS of equation ===",show x,"=== RHS of equation ===",show y])
    (x==y)

infix 4 .=.


setEq
  :: (Ord (Element xs),
      Show (Element xs),
      AsList xs,
      AsList xs1,
      Element xs1 ~ Element xs) =>
     xs -> xs1 -> Property
setEq (asList -> xs) (asList -> ys) = 
    printTestCase (unlines ["setEq:","=== FIRST SET ===",show xs,"=== SECOND SET ===",show ys]) $
    (S.fromList xs == S.fromList ys)


isSubset :: (Show a, Ord a) => [a] -> [a] -> Property
isSubset x y = 
    printTestCase (unlines ["isSubset:","=== FIRST SET ===",show x,"=== SECOND SET ===",show y])
    (S.fromList x `S.isSubsetOf` S.fromList y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Gr a b) where
    arbitrary = do
        ns <- (S.toList . S.fromList) `fmap` arbitrary :: Gen [Node]
        let nNodes = length ns
        lns <- zip ns `fmap` vector nNodes
        nEdges <- choose (0,nNodes^(2::Int))
        
        les <- let g = vectorOf nEdges (elements ns) 
               in liftM3 zip3 g g (vector nEdges)

        return (mkGraph lns les)



conjoin' :: Testable a => [a] -> Gen Prop
conjoin' [] = label "conjoin' []" True
conjoin' ((V.fromList . fmap property) -> ps) =
      (ps V.!) =<< choose (0,V.length ps-1) 
        

noDupes
  :: (Ord (Element xs), Show (Element xs), AsList xs) =>
     xs -> Property
noDupes (asList -> xs) = 
    printTestCase ("Checking duplicate-freeness of "++ show xs) $

    S.size (S.fromList xs) .=. length xs 


elementsV :: (Ord b, VG.Vector v b) => v b -> Gen b
elementsV v | VG.null v = error "elementsV: empty vector"
elementsV v = do
    i <- choose (0,VG.maxIndex v)
    return (v VG.! i)

generateUntilRight
  :: (Show (L a), SubSumTy a) => Gen a -> Gen (R a)
generateUntilRight g = fromRight <$> (g `suchThat` isRight)

generateUntilJust :: Gen (Maybe b) -> Gen b
generateUntilJust g = fromJust <$> (g `suchThat` isJust)


-- arbitraryDivisor :: Integer -> Gen Integer
-- arbitraryDivisor i =
--     let
--         factorisation = factorise i
--     in
--         do
--             powers <- mapM (\n -> choose (0,n)) (map snd factorisation)
--             return (product [ p ^ k | (p,k) <- zip (map fst factorisation) powers ])


-- arbitraryConvexCombination v 
--     | VG.null v = assert False undefined
--     | otherwise = do
-- 
--         (coeffs,sum) <- (do
--             coeffs <- vectorOf (VG.length v) (choose (0,1)) 
--             return (coeffs, sum coeffs))
-- 
--                         `suchThat` (
-- 
-- 
-- 

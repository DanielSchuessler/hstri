{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module QuickCheckUtil where

import Test.QuickCheck
import Control.Monad
import qualified Data.Set as S
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph
import qualified Data.Vector as V

cart ::  Monad m => m a1 -> m a2 -> m (a1, a2)
cart = liftM2 (,)

forAllElements :: (Testable prop, Show a) => [a] -> (a -> prop) -> Property
forAllElements [] _ = label "Vacuously true (empty domain)" True
forAllElements xs p = forAll (elements xs) p

forAllElements2 :: (Testable prop, Show a1, Show a2) =>[a1] -> [a2] -> ((a1, a2) -> prop) -> Property
forAllElements2 xs ys p = forAllElements (xs `cart` ys) p


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
    printTestCase (unlines ["Equality failed:","=== FIRST ELEMENT ===",show x,"=== SECOND ELEMENT ===",show y])
    (x==y)

infix 4 .=.


setEq :: (Show a, Ord a) => [a] -> [a] -> Property
setEq x y = 
    printTestCase (unlines ["Sets aren't equal:","=== FIRST SET ===",show x,"=== SECOND SET ===",show y])
    (S.fromList x == S.fromList y)

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
        

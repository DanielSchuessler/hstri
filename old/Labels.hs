{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module Simplicial.Labels where

import Control.Applicative
import Control.Arrow
import Data.Map as Map
import Data.Monoid
import Data.Vect.Double
import DisjointUnion
import Element
import Simplicial.GraphComplex
import HomogenousTuples
import Math.Groups.S3
import Simplicial.AnySimplex
import Simplicial.DeltaSet
import TypeLevel.TF.Fun
import TypeLevel.TF.Bool
import PreRenderable

newtype SimplexLabels a l = SimplexLabels { runSimplexLabels :: forall n. Nat n => a n -> l n }




instance DisjointUnionable (SimplexLabels a l) (SimplexLabels b l) where
    type DisjointUnion (SimplexLabels a l) (SimplexLabels b l) = SimplexLabels (Either1 a b) l

    disjointUnion a b =
        SimplexLabels (runSimplexLabels a `either1` runSimplexLabels b)




data LabeledDeltaSet a l = LabeledDeltaSet {
    lds_ds :: DeltaSet a,
    lds_sl :: SimplexLabels a l
}

instance (ShowN a) => Show (LabeledDeltaSet a l) where
    showsPrec prec = showsPrec prec . lds_ds


instance DisjointUnionable (LabeledDeltaSet a l) (LabeledDeltaSet b l) where
    type DisjointUnion  (LabeledDeltaSet a l) (LabeledDeltaSet b l) =
        LabeledDeltaSet (Either1 a b) l

    disjointUnion (LabeledDeltaSet a l) (LabeledDeltaSet a' l') =
        LabeledDeltaSet (disjointUnion a a') (disjointUnion l l')

simplbl :: Nat n => LabeledDeltaSet a l -> (a n) -> l   n
simplbl = runSimplexLabels . lds_sl

-- data OneElSequence x n
-- type instance OneElSequence x n   n' = IfEq n n' x () 
-- 
-- addSimplexLabels :: Nat n => 
--     n -> (a   n -> l) 
--     -> DeltaSet a 
--     -> LabeledDeltaSet a (OneElSequence l n)
-- mapOneElementSeqLabels :: (Nat n) => n -> (l -> l') -> LabeledDeltaSet a (OneElSequence l n) -> LabeledDeltaSet a (OneElSequence l' n)
-- mapOneElementSeqLabels n f = mapSimplexLabels (\n' x -> caseEqNat n n' (f x) ())

-- addSimplexLabels n f a =
--     LabeledDeltaSet a
--         (SimplexLabels (\x -> caseEqNat n n' (f x) ()))


mapSimplexLabels :: 
    (forall n. Nat n => l n -> l' n) 
    -> LabeledDeltaSet a l 
    -> LabeledDeltaSet a l'
mapSimplexLabels g = mapSimplexLabelsWithSimplices (\_ -> g)


--(LabeledDeltaSet a (SimplexLabels f)) =
--    LabeledDeltaSet a (SimplexLabels (\n -> g n . f n))

mapSimplexLabelsWithSimplices :: 
       (forall n. Nat n => a n -> l n -> l' n) 
    -> LabeledDeltaSet a l 
    -> LabeledDeltaSet a l'
mapSimplexLabelsWithSimplices g (LabeledDeltaSet a (SimplexLabels f)) =
    LabeledDeltaSet a (SimplexLabels (\x -> g x (f x)))

mapSimplexLabelsAt :: forall a l n. Nat n =>
    n
    -> (l   n -> l   n) 
    -> LabeledDeltaSet a l
    -> LabeledDeltaSet a l
mapSimplexLabelsAt _ f a = mapSimplexLabels (\(l :: l n') -> caseEqNat (undefined :: n) (undefined :: n') (f l) l) a 

mapSimplexLabelsWithSimplicesAt :: forall a l n. Nat n =>
    n 
    -> (a   n -> l   n -> l   n) 
    -> LabeledDeltaSet a l
    -> LabeledDeltaSet a l
mapSimplexLabelsWithSimplicesAt _ f a = 
    mapSimplexLabelsWithSimplices (\(x :: a n') l -> caseEqNat (undefined :: n) (undefined :: n') (f x l) l) a 

instance OrdN a => Subdivisible (LabeledDeltaSet a l) where
    type BarycentricSubdivision (LabeledDeltaSet a l) =
        LabeledDeltaSet (BCSFace a) (BCSFace l)

    bary (LabeledDeltaSet a l) =
        LabeledDeltaSet 
            (bary a)
            (SimplexLabels (\p ->
                mapEPath 
                    (\(AnySimplex x) -> AnySimplex (runSimplexLabels l x)) 
                    id 
                    p))


-- 
-- 
-- -- transformCoordsM :: SimplexLabels a => Proj4 -> a -> LabeledDeltaSet a
-- -- transformCoordsM m = mapSimplexLabels (trim . (.* fromProjective m) . extendWith 1)
-- 
-- -- instance SimplexLabels (DS Vec3 s1 s2 s3) where
-- --     simplbl _ = id
-- 
--

data CoordLabelsF n where
    CoordLabelsF0 :: Vec3 -> CoordLabelsF N0
    CoordLabelsF1 :: CoordLabelsF N1
    CoordLabelsF2 :: Maybe TriangleLabel -> CoordLabelsF N2
    CoordLabelsF3 :: CoordLabelsF (S (S (S n)))



type WithCoords a = LabeledDeltaSet a CoordLabelsF

addCoordFunc :: forall a. (Vert a -> Vec3) -> (Tri a -> Maybe TriangleLabel) -> DeltaSet a -> WithCoords a
addCoordFunc f0 f2 a = LabeledDeltaSet a (SimplexLabels (\(x :: a n) ->
    caseNat3 (undefined :: n) 
        (CoordLabelsF0 $ f0 x)
        CoordLabelsF1
        (CoordLabelsF2 $ f2 x)
        (const CoordLabelsF3)))
        

vertlbl :: LabeledDeltaSet a l -> (a Z) -> l Z
vertlbl = simplbl


instance Coords (WithCoords a) where
    transformCoords f = mapSimplexLabelsAt n0 (\(CoordLabelsF0 v) -> CoordLabelsF0 (f v))



setTriangleLabel
  :: Eq (a N2) => a N2
     -> TriangleLabel -> WithCoords a -> WithCoords a
setTriangleLabel tri (newlbl :: TriangleLabel) a = 
    mapSimplexLabelsWithSimplicesAt n2 (\x l ->
        if x == tri 
           then CoordLabelsF2 (Just newlbl)
           else l)
                             a



getCoords :: WithCoords a -> a Z -> Vec3
getCoords a x = case vertlbl a x of
                     CoordLabelsF0 v -> v

getTriangleLabel
  :: WithCoords a -> a N2 -> Maybe TriangleLabel
getTriangleLabel a x = case simplbl a x of
                            CoordLabelsF2 tl -> tl

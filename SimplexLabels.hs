{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module SimplexLabels where

import Control.Applicative
import Control.Arrow
import Data.Map as Map
import Data.Vect.Double
import DeltaSet
import DisjointUnion
import Element
import GraphComplex
import HomogenousTuples
import TypeLevel.TF


newtype SimplexLabels a l = SimplexLabels { runSimplexLabels :: forall n. Nat n => n -> a:$n -> l:$n }




instance DisjointUnionable (SimplexLabels a l) (SimplexLabels b l) where
    type DisjointUnion (SimplexLabels a l) (SimplexLabels b l) = SimplexLabels (EitherSequence a b) l

    disjointUnion a b =
        SimplexLabels (\n -> runSimplexLabels a n ||| runSimplexLabels b n)




data LabeledDeltaSet a l = LabeledDeltaSet {
    lds_ds :: DeltaSet a,
    lds_sl :: SimplexLabels a l
}

instance (ShowN a) => Show (LabeledDeltaSet a l) where
    showsPrec prec = showsPrec prec . lds_ds


instance DisjointUnionable (LabeledDeltaSet a l) (LabeledDeltaSet b l) where
    type DisjointUnion  (LabeledDeltaSet a l) (LabeledDeltaSet b l) =
        LabeledDeltaSet (EitherSequence a b) l

    disjointUnion (LabeledDeltaSet a l) (LabeledDeltaSet a' l') =
        LabeledDeltaSet (disjointUnion a a') (disjointUnion l l')

simplbl :: Nat n => LabeledDeltaSet a l -> n -> (a :$ n) -> l :$ n
simplbl = runSimplexLabels . lds_sl

data OneElSequence x n
type instance OneElSequence x n :$ n' = IfEq n n' x () 

addSimplexLabels :: Nat n => 
    n -> (a :$ n -> l) 
    -> DeltaSet a 
    -> LabeledDeltaSet a (OneElSequence l n)

addSimplexLabels n f a =
    LabeledDeltaSet a
        (SimplexLabels (\n' x -> caseEqNat n n' (f x) ()))


mapSimplexLabels :: 
    (forall n. Nat n => n -> l:$n -> l':$n) 
    -> LabeledDeltaSet a l 
    -> LabeledDeltaSet a l'
mapSimplexLabels g = mapSimplexLabelsWithSimplices (\n -> const (g n))


--(LabeledDeltaSet a (SimplexLabels f)) =
--    LabeledDeltaSet a (SimplexLabels (\n -> g n . f n))

mapSimplexLabelsWithSimplices :: 
    (forall n. Nat n => n -> a :$ n -> l:$n -> l':$n) 
    -> LabeledDeltaSet a l 
    -> LabeledDeltaSet a l'
mapSimplexLabelsWithSimplices g (LabeledDeltaSet a (SimplexLabels f)) =
    LabeledDeltaSet a (SimplexLabels (\n x -> g n x (f n x)))

mapSimplexLabelsAt :: Nat n =>
    n -> (l :$ n -> l :$ n) 
    -> LabeledDeltaSet a l
    -> LabeledDeltaSet a l
mapSimplexLabelsAt n f a = mapSimplexLabels (\n' x -> caseEqNat n n' (f x) x) a 

instance OrdN a => Subdivisible (LabeledDeltaSet a l) where
    type BarycentricSubdivision (LabeledDeltaSet a l) =
        LabeledDeltaSet (BCSFace' a) (BCSFace' l)

    bary (LabeledDeltaSet a l) =
        LabeledDeltaSet 
            (bary a)
            (SimplexLabels (\_ p ->
                mapEPath 
                    (\(AnySimplex n' x) -> AnySimplex n' (runSimplexLabels l n' x)) 
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

type WithCoords a = LabeledDeltaSet a (OneElSequence Vec3 N0)

addCoordFunc :: (Vert a -> Vec3) -> DeltaSet a -> WithCoords a
addCoordFunc f = addSimplexLabels n0 f 

vertlbl :: LabeledDeltaSet a l -> (a :$ Z) -> l :$ Z
vertlbl a = simplbl a n0

class Coords t where
    transformCoords :: (Vec3 -> Vec3) -> t -> t

instance Coords (WithCoords a) where
    transformCoords = mapSimplexLabelsAt n0

mapOneElementSeqLabels :: (Nat n) => n -> (l -> l') -> LabeledDeltaSet a (OneElSequence l n) -> LabeledDeltaSet a (OneElSequence l' n)
mapOneElementSeqLabels n f = mapSimplexLabels (\n' x -> caseEqNat n n' (f x) ())



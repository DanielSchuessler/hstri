{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts, FunctionalDependencies, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module Simplicial.DeltaSet1(
    module TypeLevel.TF.Nat.Small,
    module HomogenousTuples,
    module FaceClasses,
    module Element,

    DeltaSet1(..),
    AnySimplex1,
    OneSkeletonable(..),
    foldAnySimplex1,
    vertToAnySimplex1,
    edToAnySimplex1
    ) where

import TypeLevel.TF.Nat.Small
import HomogenousTuples
import FaceClasses
import Element
import Control.Arrow


class (Vertices s, Edges s) => 
    DeltaSet1 s where

    faces10 :: s -> Arc s -> Pair (Vert s)


vertToAnySimplex1 :: v -> AnySimplex1 v e
vertToAnySimplex1 = AnySimplex1 . Left
edToAnySimplex1 :: e -> AnySimplex1 v e
edToAnySimplex1 = AnySimplex1 . Right

newtype AnySimplex1 v e = AnySimplex1 (Either v e)
    deriving(Show)

foldAnySimplex1 :: 
    (v -> r) -> (e -> r) -> AnySimplex1 v e -> r
foldAnySimplex1 kv ke (AnySimplex1 x) = (kv ||| ke) x


class OneSkeletonable a where
    oneSkeleton :: a -> a


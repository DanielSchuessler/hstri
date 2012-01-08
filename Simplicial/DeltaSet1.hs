{-# LANGUAGE FlexibleContexts, FunctionalDependencies, TypeFamilies #-}
module Simplicial.DeltaSet1(
    module Simplicial.Simplex,
    module TypeLevel.TF.Nat.Small,
    module HomogenousTuples,
    module FaceClasses,
    module Element,

    DeltaSet1(..),
    AnySimplex1,
    OneSkeletonable(..)
    ) where

import TypeLevel.TF.Nat.Small
import Simplicial.Simplex
import HomogenousTuples
import FaceClasses
import Element


class (Vertices s, Edges s) => 
    DeltaSet1 s where

    faces10 :: s -> Arc s -> Pair (Vert s)



type AnySimplex1 s = Either (Vert s) (Ed s)

class OneSkeletonable a where
    oneSkeleton :: a -> a


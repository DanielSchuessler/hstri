{-# LANGUAGE NoMonomorphismRestriction, TupleSections, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts, FunctionalDependencies, TypeFamilies #-}
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
    edToAnySimplex1,
    anySimplex1s,
    faces10Ascending,
    EdgesContainingVertex_Cache,
    lookupEdgesContainingVertex,
    mkECVC
    ) where

import TypeLevel.TF.Nat.Small
import HomogenousTuples
import FaceClasses
import Element
import Control.Arrow
import qualified Data.Map as M
import Control.Applicative
import PrettyUtil


class (Vertices s, Edges s) => 
    DeltaSet1 s where

    faces10 :: s -> Arc s -> Pair (Vert s)


vertToAnySimplex1 :: v -> AnySimplex1 v e
vertToAnySimplex1 = AnySimplex1 . Left
edToAnySimplex1 :: e -> AnySimplex1 v e
edToAnySimplex1 = AnySimplex1 . Right

newtype AnySimplex1 v e = AnySimplex1 (Either v e)
    deriving(Show)

instance (Pretty v, Pretty e) => Pretty (AnySimplex1 v e) where
    prettyPrec prec = foldAnySimplex1 (prettyPrec prec) (prettyPrec prec)

foldAnySimplex1 :: 
    (v -> r) -> (e -> r) -> AnySimplex1 v e -> r
foldAnySimplex1 kv ke (AnySimplex1 x) = (kv ||| ke) x


class OneSkeletonable a where
    oneSkeleton :: a -> a

faces10Ascending
  :: DeltaSet1 s =>
     s -> Ed s -> (Pair (Vert s))
faces10Ascending = fmap reverse2 . faces10

newtype EdgesContainingVertex_Cache vert ed = 
    ECVC { lookupEdgesContainingVertex :: vert -> [(ed,Int)] } 

mkECVC
  :: (Ord (Vert s), DeltaSet1 s) => s -> EdgesContainingVertex_Cache (Vert s) (Ed s)
mkECVC s = ECVC (m M.!)
    where
        m = M.fromListWith (++)
                    . concatMap (\e -> 
                        asList 
                            (zipTuple2 
                                (faces10 s e) 
                                (map2 ((:[]) . (e,)) (0,1))))
                    $ edgeList s
                


anySimplex1s
  :: (Vertices s, Edges s) => s -> [AnySimplex1 (Vert s) (Ed s)]
anySimplex1s = 
    (++)
    <$> (map vertToAnySimplex1 . vertexList) 
    <*> (map edToAnySimplex1 . edgeList)

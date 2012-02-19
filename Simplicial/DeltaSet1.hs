{-# LANGUAGE FlexibleInstances, TemplateHaskell, NoMonomorphismRestriction, TupleSections, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts, FunctionalDependencies, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
-- {-# OPTIONS -ddump-deriv #-}
module Simplicial.DeltaSet1(
    module HomogenousTuples,
    module FaceClasses,
    module Element,
    module Data.SumType,
    module Data.Tuple.Index,

    DeltaSet1,
    face10,
    AnySimplex1,
    OneSkeletonable(..),
    OrdToDim1,
    ShowToDim1,
    foldAnySimplex1,
    biMapAnySimplex1,
    vertToAnySimplex1,
    edToAnySimplex1,
    anySimplex1s,
    mkECVC,
--     UnitInterval(..),
    UnitIntervalPoint
    ) where

import HomogenousTuples
import FaceClasses
import Element
import qualified Data.Map as M
import Control.Applicative
import PrettyUtil
import Data.SumType
import Language.Haskell.TH.Lift
import ShortShow
import Data.Tuple.Index
import MathUtil


class (Vertices s, Edges s, Vert s ~ Vert (Ed s), EdgeLike (Ed s)) => DeltaSet1 s where


face10 :: (Vertices e, Verts e ~ (v, v)) => e -> Index2 -> v
face10 = tupleToFun2 . vertices

vertToAnySimplex1 :: v -> AnySimplex1 v e
vertToAnySimplex1 = left'
edToAnySimplex1 :: e -> AnySimplex1 v e
edToAnySimplex1 = right'

type instance L (AnySimplex1 v e) = v
type instance R (AnySimplex1 v e) = e

newtype AnySimplex1 v e = AnySimplex1 (Either v e)
    deriving(Show,SubSumTy,SuperSumTy,Eq,Ord)

instance (Pretty v, Pretty e) => Pretty (AnySimplex1 v e) where
    prettyPrec prec = foldAnySimplex1 (prettyPrec prec) (prettyPrec prec)

foldAnySimplex1 :: 
    (v -> r) -> (e -> r) -> AnySimplex1 v e -> r
foldAnySimplex1 = either'


class OneSkeletonable a where
    oneSkeleton :: a -> a



mkECVC
  :: (Ord v,
      Vertices (Element (Eds s)),
      Edges s,
      Verts (Element (Eds s)) ~ (v, v)) =>
     s -> M.Map v [(Ed s, Index2)]
mkECVC s = m
    where
        m = M.fromListWith (++)
                    . concatMap (\e -> 
                        toList2 
                            (zipTuple2 
                                (vertices e) 
                                (map2 ((:[]) . (e,)) allIndex2' )))
                    $ edgeList s
                


anySimplex1s
  :: (Vertices s, Edges s) => s -> [AnySimplex1 (Vert s) (Ed s)]
anySimplex1s = 
    (++)
    <$> (map vertToAnySimplex1 . vertexList) 
    <*> (map edToAnySimplex1 . edgeList)


biMapAnySimplex1
  :: (v -> v') -> (e -> e') -> AnySimplex1 v e -> AnySimplex1 v' e'
biMapAnySimplex1 = (++++)

instance (ShortShow v, ShortShow e) => ShortShow (AnySimplex1 v e) where
    shortShow = foldAnySimplex1 shortShow shortShow 


deriveLiftMany [''AnySimplex1]

-- data UnitInterval = UnitInterval
-- 
-- instance Vertices UnitInterval where
--     type Verts UnitInterval = Pair UnitIntervalPoint
--     vertices UnitInterval = (0,1)
-- 
-- instance Edges UnitInterval where
--     type Eds UnitInterval = OneTuple UnitInterval
--     edges UnitInterval = OneTuple UnitInterval
-- 
-- instance DeltaSet1 UnitInterval where

class (Show (Vert s), Show (Ed s)) => ShowToDim1 s
instance (Show (Vert s), Show (Ed s)) => ShowToDim1 s

class (Ord (Vert s), Ord (Ed s)) => OrdToDim1 s
instance (Ord (Vert s), Ord (Ed s)) => OrdToDim1 s

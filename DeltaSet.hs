{-# LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS -Wall #-}
module DeltaSet where

import HomogenousTuples
import Data.Map as Map
import Element
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Vect.Double.Base

#include "macros.h"

type family Vert a

class DeltaSet t where
    type Delta1 t
    type Delta2 t
    type Delta3 t

    getDS :: t -> DS (Vert t) (Delta1 t) (Delta2 t) (Delta3 t) 



data DS s0 s1 s2 s3 = DS {
    -- | The first component of the result must be the triangle obtained by leaving out the first vertex, and so on
    faces10_ :: s1 -> Pair s0,
    faces21_ :: s2 -> Triple s1,
    faces32_ :: s3 -> Quadruple s2,
    s0_ :: [s0],
    s1_ :: [s1],
    s2_ :: [s2],
    s3_ :: [s3],
    super01_ :: s0 -> [s1],
    super12_ :: s1 -> [s2],
    super23_ :: s2 -> [s3],
    dimension_ :: Maybe Int
}

type instance Vert (DS s0 s1 s2 s3) = s0

instance DeltaSet (DS s0 s1 s2 s3) where
    type Delta1 (DS s0 s1 s2 s3) = s1
    type Delta2 (DS s0 s1 s2 s3) = s2
    type Delta3 (DS s0 s1 s2 s3) = s3
    getDS = id

faces10 :: DeltaSet a => a -> Delta1 a -> Pair (Vert a)
faces10 = faces10_ . getDS

faces21 :: DeltaSet a => a -> Delta2 a -> Triple (Delta1 a)
faces21 = faces21_ . getDS

faces32 :: DeltaSet a => a -> Delta3 a -> Quadruple (Delta2 a)
faces32 = faces32_ . getDS

s0 :: DeltaSet a => a -> [Vert a]
s0 = s0_ . getDS
s1 :: DeltaSet a => a -> [Delta1 a]
s1 = s1_ . getDS
s2 :: DeltaSet a => a -> [Delta2 a]
s2 = s2_ . getDS
s3 :: DeltaSet a => a -> [Delta3 a]
s3 = s3_ . getDS

super01 :: DeltaSet a => a -> Vert a -> [Delta1 a]
super01 = super01_ . getDS

super12 :: DeltaSet a => a -> Delta1 a -> [Delta2 a]
super12 = super12_ . getDS

super23 :: DeltaSet a => a -> Delta2 a -> [Delta3 a]
super23 = super23_ . getDS

-- | Homogenous dimension; Nothing if inhomogenous
dimension :: DeltaSet a => a -> Maybe Int
dimension = dimension_ . getDS

faces20 :: DeltaSet a => a -> Delta2 a -> (Vert a, Vert a, Vert a)
faces20 t tr = (v2,v1,v0)
    where
        (e12,e02,_) = faces21 t tr
        (v2,v1) = faces10 t e12
        (_,v0) = faces10 t e02

faces31
  :: DeltaSet a =>
     a
     -> Delta3 a
     -> (Delta1 a, Delta1 a, Delta1 a, Delta1 a, Delta1 a, Delta1 a)
faces31 t tet = (e23,e13,e12,e03,e02,e01)
    where
        (tr123,tr023,tr013,_) = faces32 t tet
        (e23,e13,e12) = faces21 t tr123
        ( _ ,e03,e02) = faces21 t tr023
        ( _ , _ ,e01) = faces21 t tr013

faces30
  :: DeltaSet a => a -> Delta3 a -> (Vert a, Vert a, Vert a, Vert a)
faces30 t tet = (v3,v2,v1,v0)
    where
        (tr123,tr023,_,_) = faces32 t tet
        (v3,v2,v1) = faces20 t tr123
        ( _, _,v0) = faces20 t tr023


-- s0_default t = Set.unions (fmap f (s3 t))
--     where
--         f = Set.fromList . $(tupleToList 4) . faces30 t
-- 
-- s1_default t = Set.unions (fmap f (s3 t))
--     where
--         f = Set.fromList . $(tupleToList 6) . faces31 t
-- 
-- s2_default t = Set.unions (fmap f (s3 t))
--     where
--         f = Set.fromList . $(tupleToList 4) . faces32 t





data NormalDisc t ann = 
    NormalTri (Delta3 t) Int ann |
    NormalQuad (Delta3 t) Int ann


deriving instance (Show ann, Show (Delta3 t)) => Show (NormalDisc t ann)
                        

memoFun :: Ord k => (k -> a) -> [k] -> Map k a
memoFun f xs = Map.fromList [(x,f x) | x <- xs] 

memoSuper
  :: (Ord s0, Ord s1, Ord s2) => DS s0 s1 s2 s3 -> DS s0 s1 s2 s3
memoSuper d = 
    d {
            super01_ = (memoFun (super01_ d) (s0_ d) !),
            super12_ = (memoFun (super12_ d) (s1_ d) !),
            super23_ = (memoFun (super23_ d) (s2_ d) !)
    }
    

instance (Show s0, Show s1, Show s2, Show s3) => Show (DS s0 s1 s2 s3) where
    show DS{..} = 
                    concat [ 
                        f s3_ (asList . faces32_), 
                        f s2_ (asList . faces21_), 
                        f s1_ (asList . faces10_),
                        showVerts
                        ]
        where
            f :: (Show a, Show b) => [a] -> (a -> [b]) -> String
            f as faces = unlines $ do
                a <- as
                (show a ++ " -> ") : do
                    b <- faces a
                    ["    " ++ show b]

            showVerts | Just n <- dimension_, n > 0 = ""
                      | otherwise = 
                            unlines (fmap show s0_) 

                    

data DisjointUnion a b = DisjointUnion a b
    deriving (Show)

type instance (Vert (DisjointUnion a b)) = Either (Vert a) (Vert b)


class DistributeEither a b c | a b -> c, a c -> b, b c -> a where
    distributeEither :: Either a b -> c


#define MAKE_DE(F,MAP) instance DistributeEither (F a) (F b) (F (Either a b)) where distributeEither = MAP Left ||| MAP Right

MAKE_DE([],Prelude.map)
MAKE_DE(Pair,map2)
MAKE_DE(Triple,map3)
MAKE_DE(Quadruple,map4)


instance (DeltaSet a, DeltaSet b) => DeltaSet (DisjointUnion a b) where

    type Delta1 (DisjointUnion a b) = Either (Delta1 a) (Delta1 b)
    type Delta2 (DisjointUnion a b) = Either (Delta2 a) (Delta2 b)
    type Delta3 (DisjointUnion a b) = Either (Delta3 a) (Delta3 b)

    getDS (DisjointUnion (getDS -> d1) (getDS -> d2)) = DS{..} 
        where
            dimension_ = do
                n1 <- DeltaSet.dimension_ d1
                n2 <- DeltaSet.dimension_ d2
                guard (n1==n2)
                return n1

#define S(I) I = (Left <$> DeltaSet.I d1) ++ (Right <$> DeltaSet.I d2)
            S(s0_)
            S(s1_)
            S(s2_)
            S(s3_)
#define FUN(I) I = distributeEither . (DeltaSet.I d1 +++ DeltaSet.I d2)
            FUN(super01_)
            FUN(super12_)
            FUN(super23_)
            FUN(faces10_)
            FUN(faces21_)
            FUN(faces32_)

foldMapDJ
  :: (t3 -> t4 -> t)
     -> (t1 -> t3) -> (t2 -> t4) -> DisjointUnion t1 t2 -> t
foldMapDJ g f f' (DisjointUnion a b) = g (f a) (f' b)

deriveDJ :: (a -> a' -> r) -> (b -> b' -> r) -> DisjointUnion a b -> Either a' b' -> r
deriveDJ = foldMapDJ (|||)

class Coordinates a where
    coords :: a -> Vert a -> Vec3

instance (Coordinates a, Coordinates b) => Coordinates (DisjointUnion a b) where
    coords = deriveDJ coords coords

data ComplexPlus a b = ComplexPlus {
    cp_complex :: a,
    cp_extra :: b
}
    deriving Show


DERIVE_DELTA_SET(ComplexPlus a b, cp_complex) 

newtype VertexCoordFunc a = VertexCoordFunc { getVertexCoordFunc :: Vert a -> Vec3 } 

type WithVertexCoordFunc a = ComplexPlus a (VertexCoordFunc a) 

instance Coordinates (WithVertexCoordFunc a) where
    coords = getVertexCoordFunc . cp_extra


addCoordFunc :: (Vert a -> Vec3) -> a -> WithVertexCoordFunc a
addCoordFunc f a = ComplexPlus a (VertexCoordFunc f)

transformCoords :: Coordinates a => (Vec3 -> Vec3) -> a -> WithVertexCoordFunc a
transformCoords t a = addCoordFunc (t . coords a) a

-- transformCoordsM :: Coordinates a => Proj4 -> a -> WithVertexCoordFunc a
-- transformCoordsM m = transformCoords (trim . (.* fromProjective m) . extendWith 1)


instance Coordinates (DS Vec3 s1 s2 s3) where
    coords _ = id

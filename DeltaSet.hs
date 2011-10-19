{-# LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, NoMonomorphismRestriction, RecordWildCards #-}
module DeltaSet where

import TupleTH
import Data.Set as Set
import HomogenousTuples
import Data.Map as Map

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
    super23_ :: s2 -> [s3]
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

faces20 t tr = (v2,v1,v0)
    where
        (e12,e02,_) = faces21 t tr
        (v2,v1) = faces10 t e12
        (_,v0) = faces10 t e02

faces31 t tet = (e23,e13,e12,e03,e02,e01)
    where
        (tr123,tr023,tr013,_) = faces32 t tet
        (e23,e13,e12) = faces21 t tr123
        ( _ ,e03,e02) = faces21 t tr023
        ( _ , _ ,e01) = faces21 t tr013

faces30 t tet = (v3,v2,v1,v0)
    where
        (tr123,tr023,_,_) = faces32 t tet
        (v3,v2,v1) = faces20 t tr123
        ( _, _,v0) = faces20 t tr023


s0_default t = Set.unions (fmap f (s3 t))
    where
        f = Set.fromList . $(tupleToList 4) . faces30 t

s1_default t = Set.unions (fmap f (s3 t))
    where
        f = Set.fromList . $(tupleToList 6) . faces31 t

s2_default t = Set.unions (fmap f (s3 t))
    where
        f = Set.fromList . $(tupleToList 4) . faces32 t




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
    


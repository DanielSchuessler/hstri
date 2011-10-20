{-# LANGUAGE RecordWildCards, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, NamedFieldPuns, TypeFamilies, DefaultSignatures, FlexibleContexts, OverlappingInstances, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, CPP #-} 
-- | All 'Pair's, 'Triple's and 'Quadruple's are assumed to be strictly ascendingly ordered in this module.
module SimplicialComplex where

import AbstractTetrahedron
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.Map as Map
import Data.Set as Set
import Data.Vect.Double 
import Data.Vect.Double.Instances
import DeltaSet
import HomogenousTuples
import TupleTH

#include "macros.h"

isOrdered2 (v0,v1) = v0 < v1
isOrdered3 (v0,v1,v2) = v0 < v1 && v1 < v2 
isOrdered4 (v0,v1,v2,v3) = isOrdered3 (v0,v1,v2) && v2 < v3

toList6 :: (a,a,a,a,a,a) -> [a]
toList6 = $(tupleToList 6)





type SC v = DS v (Pair v) (Triple v) (Quadruple v)

simplicialSuper01 :: Eq v => [Pair v] -> v -> [Pair v]
simplicialSuper01 s1_ = \v -> Prelude.filter ($(elemTuple 2) v) s1_
simplicialSuper12 :: Eq v => [Triple v] -> Pair v -> [Triple v]
simplicialSuper12 s2_ = \e -> Prelude.filter (\t -> $(elemTuple 3) e ($(subtuples 3 2) t)) s2_
simplicialSuper23 :: Eq v => [Quadruple v] -> Triple v -> [Quadruple v]
simplicialSuper23 s3_ = \t -> Prelude.filter (\tt -> $(elemTuple 4) t ($(subtuples 4 3) tt)) s3_

simplicialFaces10_ (v0,v1) = (v1,v0)
simplicialFaces21_ (v0,v1,v2) = ((v1,v2),(v0,v2),(v1,v2))
simplicialFaces32_ (v0,v1,v2,v3) = ((v1,v2,v3),(v0,v2,v3),(v0,v1,v3),(v0,v1,v2))

fromTets :: Ord v => [Quadruple v] -> SC v
fromTets ft_tets = assert (all isOrdered4 ft_tets) $ memoSuper (DS{..})
    where
        s0_ = nub' . concatMap toList4 $ ft_tets
        s1_ = nub' . concatMap (toList6 . $(subtuples 4 2)) $ ft_tets
        s2_ = nub' . concatMap (toList4 . $(subtuples 4 3)) $ ft_tets 
        s3_ = ft_tets
        super01_ = simplicialSuper01 s1_ 
        super12_ = simplicialSuper12 s2_ 
        super23_ = simplicialSuper23 s3_ 
        faces10_ = simplicialFaces10_ 
        faces21_ = simplicialFaces21_ 
        faces32_ = simplicialFaces32_ 
        dimension_ = Just 3



fromTris :: Ord v => [Triple v] -> SC v
fromTris ft_tris = assert (all isOrdered3 ft_tris) $ memoSuper (DS{..})
    where
        s0_ = nub' . concatMap toList3 $ ft_tris
        s1_ = nub' . concatMap (toList3 . $(subtuples 3 2)) $ ft_tris
        s2_ = ft_tris
        s3_ = []
        super01_ = simplicialSuper01 s1_
        super12_ = simplicialSuper12 s2_
        super23_ _ = []
        faces10_ = simplicialFaces10_ 
        faces21_ = simplicialFaces21_ 
        faces32_ = simplicialFaces32_ 
        dimension_ = Just 2

fromEdges :: Ord v => [Pair v] -> SC v
fromEdges s1_ = assert (all isOrdered2 s1_) $ memoSuper (DS{..})
    where
        s0_ = nub' . concatMap toList2 $ s1_
        s2_ = []
        s3_ = []
        super01_ = simplicialSuper01 s1_
        super12_ _ = []
        super23_ _ = []
        faces10_ = simplicialFaces10_ 
        faces21_ = simplicialFaces21_ 
        faces32_ = simplicialFaces32_ 
        dimension_ = Just 1








-- data AbstractTet = AbstractTet
--     deriving (Show)
-- 
-- type instance Vert AbstractTet = Vertex

abstractTet :: SC Vertex
abstractTet = fromTets [allVertices']


data BaryVertex1 v = Bary0 v
                   | Bary1 (Pair v)
                    deriving(Eq,Ord,Show,Functor)

data BaryVertex2 v = BaryVertex1 (BaryVertex1 v)
                   | Bary2 (Triple v)
                    deriving(Eq,Ord,Show,Functor)

data BaryVertex3 v = BaryVertex2 (BaryVertex2 v)
                   | Bary3 (Quadruple v)

                    deriving(Eq,Ord,Show,Functor)

class (DeltaSet a, 
       Delta1 a ~ (Pair (Vert a)), 
       Delta2 a ~ (Triple (Vert a)), 
       Delta3 a ~ (Quadruple (Vert a)))

      => SimplicialComplex a

instance (DeltaSet a, 
       Delta1 a ~ (Pair (Vert a)), 
       Delta2 a ~ (Triple (Vert a)), 
       Delta3 a ~ (Quadruple (Vert a)))

      => SimplicialComplex a


bary3 :: (Ord v, SimplicialComplex a, v ~ Vert a) => 
    a -> ComplexPlus (SC (BaryVertex3 v)) a

bary3 c0 = ComplexPlus c c0 
    where
        c =
            fromTets [ $(catTuples 3 1)  
                        ( map3 BaryVertex2 $ $(catTuples 2 1)  
                                (map2 BaryVertex1 (Bary0 v, Bary1 e)) 
                                (Bary2 t)) 
                            (Bary3 tt)
                    |
                        v <- s0 c0,
                        e <- super01 c0 v,
                        t <- super12 c0 e,
                        tt <- super23 c0 t]

bary2 :: (Ord v, SimplicialComplex a, v ~ Vert a) => 
    a -> ComplexPlus (SC (BaryVertex2 v)) a

bary2 c0 = ComplexPlus c c0 
    where
        c =
            fromTris [ 
                        ( $(catTuples 2 1)  
                                (map2 BaryVertex1 (Bary0 v, Bary1 e)) 
                                (Bary2 t)) 
                    |
                        v <- s0 c0,
                        e <- super01 c0 v,
                        t <- super12 c0 e
                        ]


instance Coordinates (SC Vertex) where 
    coords = const (vlbl . fromEnum)
      where
        vlbl 0 = vec3X
        vlbl 1 = vec3Y
        vlbl 2 = vec3Z
        vlbl 3 = 1


instance (v ~ Vert a, Coordinates a) => Coordinates (ComplexPlus (SC (BaryVertex3 v)) a) where
    coords (ComplexPlus _ a) v = barycenter (fmap (coords a) v) 

instance (v ~ Vert a, Coordinates a) => Coordinates (ComplexPlus (SC (BaryVertex2 v)) a) where
    coords (ComplexPlus _ a) v = barycenter (fmap (coords a) v) 


-- 
-- -- 
-- --
-- -- 
-- -- baryGeom :: (Ord v, Vector vlbl) => SimplicialComplex3 v vlbl -> SimplicialComplex3 (BaryVertex v) vlbl
-- -- baryGeom s@SimplicialComplex3 {vlbl} = (bary s) { vlbl = barycenter . fmap vlbl }
-- -- 
-- --
-- 


class Barycenter x where
    barycenter :: x -> Vec3

instance Barycenter (BaryVertex1 Vec3) where
    barycenter (Bary0 v) = v
    barycenter (Bary1 (v0,v1)) = (1/2) *& (v0 &+ v1)

instance Barycenter (BaryVertex2 Vec3) where
    barycenter (BaryVertex1 x) = barycenter x
    barycenter (Bary2 (v0,v1,v2)) = (1/3) *& (v0 &+ v1 &+ v2)

instance Barycenter (BaryVertex3 Vec3) where
    barycenter (BaryVertex2 x) = barycenter x
    barycenter (Bary3 (v0,v1,v2,v3)) = (1/4) *& (v0 &+ v1 &+ v2 &+ v3)

edgeCoords
  :: (Coordinates a, SimplicialComplex a) => a -> [Pair Vec3]
edgeCoords c = map2 (coords c) <$> s1 c

triCoords
  :: (Coordinates a, SimplicialComplex a) => a -> [Triple Vec3]
triCoords c = map3 (coords c) <$> s2 c



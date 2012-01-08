{-# LANGUAGE TypeFamilies, FunctionalDependencies #-}
module Simplicial.DeltaSet3(
    module Simplicial.DeltaSet2,
    DeltaSet3(..),
    faces31,
    faces30,
    AnySimplex3
    )
    where

import Simplicial.DeltaSet2

class (DeltaSet2 s, Tetrahedra s) =>
    DeltaSet3 s where

    faces32 :: s -> Tet s -> Quadruple (Tri s)

type AnySimplex3 s = Either (AnySimplex2 s) (Tet s) 


faces31
  :: DeltaSet3 s
     => s
     -> Tet s
     -> (Sextuple (Ed s))
faces31 t tet = (e23,e13,e12,e03,e02,e01)
    where
        (tr123,tr023,tr013,_) = faces32 t tet
        (e23,e13,e12) = faces21 t tr123
        ( _ ,e03,e02) = faces21 t tr023
        ( _ , _ ,e01) = faces21 t tr013

faces30
  :: DeltaSet3 s => s -> Tet s -> (Quadruple (Vert s))
faces30 t tet = (v3,v2,v1,v0)
    where
        (tr123,tr023,_,_) = faces32 t tet
        (v3,v2,v1) = faces20 t tr123
        ( _, _,v0) = faces20 t tr023 

{-# LANGUAGE TemplateHaskell, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simplicial.DeltaSet3(
    module Simplicial.DeltaSet2,
    DeltaSet3(..),
    faces31,
    faces30,
    AnySimplex3,
    foldAnySimplex3,
    anySimplex2To3
    )
    where

import Simplicial.DeltaSet2
import Control.Arrow
import Language.Haskell.TH.Lift

class (DeltaSet2 s, Tetrahedra s) =>
    DeltaSet3 s where

    faces32 :: s -> Tet s -> Quadruple (Tri s)

newtype AnySimplex3 v e t tet = AnySimplex3 (Either (AnySimplex2 v e t) tet)
   deriving(SuperSumTy,SubSumTy)

type instance L (AnySimplex3 v e t tet) = AnySimplex2 v e t 
type instance R (AnySimplex3 v e t tet) = tet


foldAnySimplex3 :: 
    (v -> r) -> (e -> r) -> (t -> r) -> (tet -> r) -> AnySimplex3 v e t tet -> r
foldAnySimplex3 kv ke kt ktet (AnySimplex3 x) = (foldAnySimplex2 kv ke kt ||| ktet) x

anySimplex2To3 :: AnySimplex2 v e t -> AnySimplex3 v e t tet
anySimplex2To3 = left'

tetToAnySimplex3 :: tet -> AnySimplex3 v e t tet
tetToAnySimplex3 = right'

foldAnySimplex3' k2 ktet (AnySimplex3 x) = either k2 ktet x

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


  
deriveLiftMany [''AnySimplex3]

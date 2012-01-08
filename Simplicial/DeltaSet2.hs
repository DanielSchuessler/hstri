{-# LANGUAGE FlexibleContexts, TypeFamilies, FunctionalDependencies #-}
{-# OPTIONS -Wall #-}
module Simplicial.DeltaSet2(
    module Simplicial.DeltaSet1,
    DeltaSet2(..),
    AnySimplex2,
    faces20,
    faces20Ascending,
    -- * Testing
    polyprop_faces21
    ) where

import Simplicial.DeltaSet1
import QuickCheckUtil
import Test.QuickCheck

class (DeltaSet1 s, Triangles s) => 
    DeltaSet2 s where

    faces21 :: s -> Tri s -> Triple (Arc s)

type AnySimplex2 s = Either (AnySimplex1 s) (Tri s) 

polyprop_faces21
  :: (Eq (Element (Verts s)),
      Show tris,
      Show (Element (Verts s)),
      DeltaSet2 s,
      Tris s ~ [tris]) =>
     s -> Property
polyprop_faces21 s = 
    forAllElements (triangles s)
        (\t -> case faces21 s t of
                    es ->
                        case map3 (faces10 s) es of
                             ( (v0_0,v1_0)
                              ,(v0_1,v1_1)
                              ,(v0_2,v1_2)
                              ) ->

                                 v1_1 .=. v1_2 -- 0
                                 .&.
                                 v1_0 .=. v0_2 -- 1
                                 .&.
                                 v0_0 .=. v0_1 -- 2
                                 )
                                 
                                    



faces20
  :: DeltaSet2 s =>
     s -> Tri s -> (Vert s, Vert s, Vert s)
faces20 t tr = (v2,v1,v0)
    where
        (e12,e02,_) = faces21 t tr
        (v2,v1) = faces10 t e12
        (_,v0) = faces10 t e02

faces20Ascending
  :: DeltaSet2 s =>
     s -> Tri s -> (Vert s, Vert s, Vert s)
faces20Ascending t = reverse3 . faces20 t 


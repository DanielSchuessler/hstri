{-# LANGUAGE FlexibleContexts, TypeFamilies, FunctionalDependencies #-}
{-# OPTIONS -Wall #-}
module Simplicial.DeltaSet2(
    module Simplicial.DeltaSet1,
    DeltaSet2(..),
    AnySimplex2,
    AnySimplex2Of,
    faces20,
    faces20Ascending,
    foldAnySimplex2,
    vertToAnySimplex2,
    edToAnySimplex2,
    triToAnySimplex2,
    foldAnySimplex2',
    anySimplex1To2,

    -- * Testing
    polyprop_faces21
    ) where

import Simplicial.DeltaSet1
import QuickCheckUtil
import Test.QuickCheck
import Control.Arrow

class (DeltaSet1 s, Triangles s) => 
    DeltaSet2 s where

    faces21 :: s -> Tri s -> Triple (Arc s)

newtype AnySimplex2 v e t = AnySimplex2 (Either (AnySimplex1 v e) t) 
    deriving(Show)

type AnySimplex2Of s = AnySimplex2 (Vert s) (Ed s) (Tri s)


foldAnySimplex2'
  :: (AnySimplex1 v e -> r) -> (t -> r) -> AnySimplex2 v e t -> r
foldAnySimplex2' kve kt (AnySimplex2 x) = (kve ||| kt) x 

foldAnySimplex2 :: 
    (v -> r) -> (e -> r) -> (t -> r) -> AnySimplex2 v e t -> r
foldAnySimplex2 kv ke kt (AnySimplex2 x) = (foldAnySimplex1 kv ke ||| kt) x 

anySimplex1To2 :: AnySimplex1 v e -> AnySimplex2 v e t
anySimplex1To2 = AnySimplex2 . Left

vertToAnySimplex2 :: v -> AnySimplex2 v e t
vertToAnySimplex2 = anySimplex1To2 . vertToAnySimplex1

edToAnySimplex2 :: e -> AnySimplex2 v e t
edToAnySimplex2 = anySimplex1To2 . edToAnySimplex1

triToAnySimplex2 :: t -> AnySimplex2 v e t
triToAnySimplex2 = AnySimplex2 . Right

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


{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simplicial.DeltaSet3(
    module Simplicial.DeltaSet2,
    SatisfiesSimplicialIdentities3,
    DeltaSet3,
    AnySimplex3,
    foldAnySimplex3,
    anySimplex2To3
    )
    where

import Simplicial.DeltaSet2
import Control.Arrow
import Language.Haskell.TH.Lift

 -- | LAW: @forall i j. i >= j ==> 'triangleGetEdgeAt' i . 'tetrahedronGetTriangleAt' j == 'triangleGetEdgeAt' j . 'tetrahedronGetTriangleAt' (i+1)@  
--
-- (modulo index type conversions)
class (Triangles tet, Tris tet ~ Quadruple (Tri tet), Edges (Tri tet), Eds (Tri tet) ~ Triple (Ed (Tri tet)), SatisfiesSimplicialIdentities2 (Tri tet)) => SatisfiesSimplicialIdentities3 tet


class (DeltaSet2 s, Tetrahedra s, Tri (Tet s) ~ Tri s, SatisfiesSimplicialIdentities3 (Tet s)) => DeltaSet3 s where


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


  
deriveLiftMany [''AnySimplex3]

{-# LANGUAGE TupleSections, FlexibleInstances, NoMonomorphismRestriction, FlexibleContexts, TemplateHaskell, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Simplicial.DeltaSet3(
    module Simplicial.DeltaSet2,
    PreDeltaSet3,
    SatisfiesSimplicialIdentities3,
    DeltaSet3,
    DeltaSetMorphism3(..),
    AnySimplex3,
    foldAnySimplex3,
    anySimplex2To3,
    -- * Superclass defaults
    DefaultVertsOfTet,
    defaultVerticesOfTet,
    DefaultEdsOfTet,
    defaultEdgesOfTet,
    -- * Lookups
    TetsContainingTris_Cache(..),
    mkTCTC
    )
    where

import Simplicial.DeltaSet2
import Control.Arrow
import Language.Haskell.TH.Lift
import Control.Exception
import FileLocation
import HomogenousTuples
import qualified Data.Map as M
import Data.Map(Map)


class (PreDeltaSet2 s, Tetrahedra s, Tri (Tet s) ~ Tri s, TetrahedronLike (Tet s)) => PreDeltaSet3 s where
instance (PreDeltaSet2 s, Tetrahedra s, Tri (Tet s) ~ Tri s, TetrahedronLike (Tet s)) => PreDeltaSet3 s where


-- | LAW: @forall i j. i >= j ==> 'triangleGetEdgeAt' i . 'tetrahedronGetTriangleAt' j == 'triangleGetEdgeAt' j . 'tetrahedronGetTriangleAt' (i+1)@  
--
-- (modulo index type conversions)
class (TetrahedronLike tet, SatisfiesSimplicialIdentities2 (Tri tet)) => SatisfiesSimplicialIdentities3 tet


class (DeltaSet2 s, PreDeltaSet3 s, SatisfiesSimplicialIdentities3 (Tet s)) => DeltaSet3 s where


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


class DeltaSetMorphism2 (Tri tet) (Tri tet') f => DeltaSetMorphism3 tet tet' f | f -> tet tet' where
    mapTet :: f -> tet -> tet'

type DefaultVertsOfTet tet = Quadruple (Vert (Ed (Tri tet)))

-- | Derives a 'vertices' function for a tetrahedron-like type, given that 'triangles' is implemented, 'edges' is implemented for @'Tri' tet@ and 'vertices' is implemented for @'Ed' ('Tri' tet)@.
defaultVerticesOfTet
  :: (SatisfiesSimplicialIdentities3 tet) =>
     tet -> DefaultVertsOfTet tet
defaultVerticesOfTet tet =
    let
        (a,b,c) = defaultVerticesOfTri (tetrahedronGetTriangleAt tet I4_0) 
        (_,_,d) = defaultVerticesOfTri (tetrahedronGetTriangleAt tet I4_1) 
    in
        (a,b,c,d)


type DefaultEdsOfTet tet = Sextuple (Ed (Tri tet))

defaultEdgesOfTet
  :: (SatisfiesSimplicialIdentities3 tet) =>
     tet -> DefaultEdsOfTet tet
defaultEdgesOfTet tet =
    let
        (ab,ac,bc) = edges (tetrahedronGetTriangleAt tet I4_0) 
        (ab',ad,bd) = edges (tetrahedronGetTriangleAt tet I4_1) 
        (bc',bd',cd) = edges (tetrahedronGetTriangleAt tet I4_3) 
    in

        (ab,ac,ad,bc,bd,cd)


newtype TetsContainingTris_Cache tri tet = 
    TCTC { lookupTetsContainingTri :: tri -> [(tet,Index4)] } 



mkTCTC
  :: (Ord (Tri s), PreDeltaSet3 s) => s -> TetsContainingTris_Cache (Tri s) (Tet s)
mkTCTC s = TCTC (flip $(indx) m)
    where
        m = M.fromListWith (++)
                    . concatMap (\t -> 
                        toList4 
                            (zipTuple4 
                                (triangles t) 
                                (map4 ((:[]) . (t,)) allIndex4')))
                    $ tetrahedronList s


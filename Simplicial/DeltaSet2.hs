{-# LANGUAGE FlexibleInstances, StandaloneDeriving, CPP, RecordWildCards, TemplateHaskell, NoMonomorphismRestriction, TupleSections, FlexibleContexts, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Simplicial.DeltaSet2(
    module Simplicial.DeltaSet1,
    SatisfiesSimplicialIdentities2,
    PreDeltaSet2,
    DeltaSet2,
    face21,
    DefaultVertsOfTri,
    defaultVerticesOfTri,
    DeltaSetMorphism2(..),

    -- * AnySimplex2
    AnySimplex2,
    AnySimplex2Of,
    foldAnySimplex2,
    vertToAnySimplex2,
    edToAnySimplex2,
    triToAnySimplex2,
    foldAnySimplex2',
    biMapAnySimplex2,
    anySimplex1To2,
    anySimplex2s,
    -- * Lookups
    TrianglesContainingEdge_Cache,
    lookupTrianglesContainingEdge,
    mkTCEC,
--     Unit2Simplex(..),
    Unit2SimplexPoint,
    -- * Generic
    GenericDeltaSet2(..),
    gds2_fromTris,


    ) where

import Simplicial.DeltaSet1
import Control.Arrow
import qualified Data.Map as M
import Control.Applicative
import PrettyUtil
import Language.Haskell.TH.Lift
import ShortShow
import MathUtil
import FileLocation
import Data.Ord
import Util


-- | LAW: @forall i j. i >= j ==> 'edgeGetVertexAt' i . 'triangleGetEdgeAt' j == 'edgeGetVertexAt' j . 'triangleGetEdgeAt' (i+1)@  
--
-- (modulo index type conversions)
class (TriangleLike t) => SatisfiesSimplicialIdentities2 t



class (DeltaSet1 s, Triangles s, Ed s ~ Ed (Tri s), TriangleLike (Tri s)) => PreDeltaSet2 s
instance (DeltaSet1 s, Triangles s, Ed s ~ Ed (Tri s), TriangleLike (Tri s)) => PreDeltaSet2 s

class (PreDeltaSet2 s, SatisfiesSimplicialIdentities2 (Tri s)) => DeltaSet2 s

class DeltaSetMorphism2 t t' f | f -> t t' where
    mapVert :: f -> Vert t -> Vert t' 
    mapEd :: f -> Ed t -> Ed t'
    mapTri :: f -> t -> t' 



face21 :: (Triangles t, Tris t ~ (e, e, e)) => t -> Index3 -> e
face21 = tupleToFun3 . triangles

newtype AnySimplex2 v e t = AnySimplex2 (Either (AnySimplex1 v e) t) 
    deriving(Show,SubSumTy,SuperSumTy,Eq,Ord)

type instance L (AnySimplex2 v e t) = AnySimplex1 v e 
type instance R (AnySimplex2 v e t) = t


instance (Pretty v, Pretty e, Pretty t) => Pretty (AnySimplex2 v e t) where
    prettyPrec prec = foldAnySimplex2' (prettyPrec prec) (prettyPrec prec)

type AnySimplex2Of s = AnySimplex2 (Vert s) (Ed s) (Tri s)


foldAnySimplex2'
  :: (AnySimplex1 v e -> r) -> (t -> r) -> AnySimplex2 v e t -> r
foldAnySimplex2' = (||||)

foldAnySimplex2 :: 
    (v -> r) -> (e -> r) -> (t -> r) -> AnySimplex2 v e t -> r
foldAnySimplex2 kv ke kt (AnySimplex2 x) = (foldAnySimplex1 kv ke ||| kt) x 

anySimplex1To2 :: AnySimplex1 v e -> AnySimplex2 v e t
anySimplex1To2 = left'

vertToAnySimplex2 :: v -> AnySimplex2 v e t
vertToAnySimplex2 = anySimplex1To2 . vertToAnySimplex1

edToAnySimplex2 :: e -> AnySimplex2 v e t
edToAnySimplex2 = anySimplex1To2 . edToAnySimplex1

triToAnySimplex2 :: t -> AnySimplex2 v e t
triToAnySimplex2 = right'


type DefaultVertsOfTri t = Triple (Vert (Ed t))

defaultVerticesOfTri
  :: (SatisfiesSimplicialIdentities2 t) =>

     t -> DefaultVertsOfTri t

-- | Derives a 'vertices' function for a triangle-like type, given that 'edges' is implemented and 'vertices' is implemented for @'Ed' t@.
defaultVerticesOfTri t = (v0,v1,v2)
    where
        (e01,e02,_) = edges t
        (v0,v1) = vertices e01
        (_,v2) = vertices e02




newtype TrianglesContainingEdge_Cache ed tri = 
    TCEC { lookupTrianglesContainingEdge :: ed -> [(tri,Index3)] } 



mkTCEC
  :: (Ord (Ed s), PreDeltaSet2 s) => s -> TrianglesContainingEdge_Cache (Ed s) (Tri s)
mkTCEC s = TCEC (flip $(indx) m)
    where
        m = M.fromListWith (++)
                    . concatMap (\t -> 
                        toList3 
                            (zipTuple3 
                                (edges t) 
                                (map3 ((:[]) . (t,)) allIndex3')))
                    $ triangleList s
                

anySimplex2s
  :: (Vertices s, Triangles s, Edges s) =>
     s -> [AnySimplex2 (Vert s) (Ed s) (Tri s)]
anySimplex2s = 
    (++)
    <$> (map anySimplex1To2 . anySimplex1s) 
    <*> (map triToAnySimplex2 . triangleList)



biMapAnySimplex2
  :: (AnySimplex1 v e -> AnySimplex1 v' e')
     -> (t -> t') -> AnySimplex2 v e t -> AnySimplex2 v' e' t'
biMapAnySimplex2 = (++++)


instance (Lift v, Lift e, Lift t) => Lift (AnySimplex2 v e t) where

    lift = either' 
            (either' 
                (\x -> [| vertToAnySimplex2 x |])
                (\x -> [| edToAnySimplex2 x |]))
            (\x -> [| triToAnySimplex2 x |])

instance (ShortShow v, ShortShow e, ShortShow t) => ShortShow (AnySimplex2 v e t) where
    shortShow = foldAnySimplex2' shortShow shortShow 

data GenericDeltaSet2 t = GDS2 {
        gds2_vertices :: [Vert t],
        gds2_edges :: [Ed t],
        gds2_triangles :: [t] 
    }

#define DERIVE(C) deriving instance (C t, C (Ed t), C (Vert t)) => C (GenericDeltaSet2 t)
DERIVE(Show)
#undef DERIVE


instance Vertices (GenericDeltaSet2 t) where
    type Verts (GenericDeltaSet2 t) = [Vert t]
    vertices = gds2_vertices

instance Edges (GenericDeltaSet2 t) where
    type Eds (GenericDeltaSet2 t) = [Ed t]
    edges = gds2_edges

instance Triangles (GenericDeltaSet2 t) where
    type Tris (GenericDeltaSet2 t) = [t]
    triangles = gds2_triangles

instance (SatisfiesSimplicialIdentities2 t) => DeltaSet1 (GenericDeltaSet2 t) 
instance (SatisfiesSimplicialIdentities2 t) => DeltaSet2 (GenericDeltaSet2 t) 


gds2_fromTris :: (Ord (Vert t), Ord (Ed t), SatisfiesSimplicialIdentities2 t) => 
    [t] -> GenericDeltaSet2 t
gds2_fromTris tris = 
    let
        eds = nub' . concatMap edgeList $ tris
        verts = nub' . concatMap vertexList $ eds
    in
        GDS2 verts eds tris

-- data Unit2Simplex = Unit2Simplex
--     deriving Show
-- 
-- instance Vertices Unit2Simplex where
--     type Verts Unit2Simplex = Triple Unit2SimplexPoint
--     vertices Unit2Simplex = faces20 Unit2Simplex Unit2Simplex
-- 
-- instance Edges Unit2Simplex where
--     type Eds Unit2Simplex = Triple (Unit2SimplexPoint,Unit2SimplexPoint)
--     edges Unit2Simplex = faces21 Unit2Simplex Unit2Simplex
-- 
-- instance Triangles Unit2Simplex where
--     type Tris Unit2Simplex = OneTuple Unit2Simplex
--     triangles Unit2Simplex = OneTuple Unit2Simplex
-- 
-- instance DeltaSet1 Unit2Simplex where
--     faces10 _ = reverse2
-- 
-- instance DeltaSet2 Unit2Simplex where
--     faces21 _ _ = ((b,c),(a,c),(a,b))
--         where
--             a = Vec2 0 0
--             b = Vec2 0 1
--             c = Vec2 1 0


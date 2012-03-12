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
    ShowToDim2,
    OrdToDim2,

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
    mkTCEC,
    SuperfaceLookup2(..),
    inheritSuperfaceLookup2,
    WithSuperfaceLookup2(..),
    wsl2_underlyingL,
    addSuperfaceLookup2,

--     Unit2Simplex(..),
    Unit2SimplexPoint,
    -- * Generic
    GenericDeltaSet2(..),
    gds2_fromTris,
    gds2


    ) where

import Simplicial.DeltaSet1
import Control.Arrow
import qualified Data.Map as M
import Data.Map(Map)
import Control.Applicative
import PrettyUtil
import Language.Haskell.TH.Lift
import Language.Haskell.TH
import ShortShow
import MathUtil
import FileLocation
import Util
import Language.Haskell.TH.Build
import THUtil
import Data.Lens.Common


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

class (ShowToDim1 s, Show (Tri s)) => ShowToDim2 s
instance (ShowToDim1 s, Show (Tri s)) => ShowToDim2 s

class (OrdToDim1 s, Ord (Tri s)) => OrdToDim2 s
instance (OrdToDim1 s, Ord (Tri s)) => OrdToDim2 s

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







mkTCEC
  :: (Ord k,
      Triangles a,
      Edges (Element (Tris a)),
      Eds (Element (Tris a)) ~ (k, k, k)) =>
     a -> Map k [(Tri a, Index3)]
mkTCEC s = m
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

instance (Pretty t, Pretty (Vert t), Pretty (Ed t)) => Pretty (GenericDeltaSet2 t) where
    prettyPrec = prettyRecordPrec 
        (\(GDS2 v e t) ->
                ("GDS2",[("gds2_vertices",pretty v), ("gds2_edges",pretty e), 
                    ("gds2_triangles",pretty t)])) 


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

instance (TriangleLike t) => DeltaSet1 (GenericDeltaSet2 t) 
instance (SatisfiesSimplicialIdentities2 t) => DeltaSet2 (GenericDeltaSet2 t) 


gds2_fromTris :: (Ord (Vert t), Ord (Ed t), SatisfiesSimplicialIdentities2 t) => 
    [t] -> GenericDeltaSet2 t
gds2_fromTris tris = 
    let
        eds = nub' . concatMap edgeList $ tris
        verts = nub' . concatMap vertexList $ eds
    in
        GDS2 verts eds tris

gds2
  :: (Vertices a,
      Triangles a,
      Edges a,
      Verts a ~ [Element (Verts t)],
      Tris a ~ [t],
      Eds a ~ [Element (Eds t)]) =>
     a -> GenericDeltaSet2 t
gds2 ds = GDS2 (vertices ds) (edges ds) (triangles ds)


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


class SuperfaceLookup2 s where 
    edsContainingVert :: s -> Vert s -> [(Ed s, Index2)] 
    trisContainingEd :: s -> Ed s -> [(Tri s, Index3)]

inheritSuperfaceLookup2
  :: (Convertible accessor ExpQ,
      Convertible sub TypeQ,
      Convertible super TypeQ) =>
     sub -> super -> accessor -> Q [Dec]
inheritSuperfaceLookup2 = 
    inheritSingleArgClass ''SuperfaceLookup2 ['edsContainingVert,'trisContainingEd] [] 

data WithSuperfaceLookup2 s =
    WithSuperfaceLookup2 {
        wsl2_underlying :: s,
        wsl2_ecvc :: Map (Vert s) [(Ed s,Index2)],
        wsl2_tcec :: Map (Ed s) [(Tri s,Index3)]
    }



deriving instance (ShowToDim2 s, Show s) => Show (WithSuperfaceLookup2 s)
instance DeltaSet1 s => DeltaSet1 (WithSuperfaceLookup2 s) 

inheritToDim2 (''WithSuperfaceLookup2 `appT'` "s") "s" 'wsl2_underlying

instance (ShowToDim1 s, OrdToDim1 s) => SuperfaceLookup2 (WithSuperfaceLookup2 s) where
    edsContainingVert = flip $indxShow . wsl2_ecvc
    trisContainingEd = flip $indxShow . wsl2_tcec



addSuperfaceLookup2 :: (OrdToDim1 s, PreDeltaSet2 s) => s -> WithSuperfaceLookup2 s
addSuperfaceLookup2 s = WithSuperfaceLookup2 s (mkECVC s) (mkTCEC s)

-- | Regenerates the lookups when set
wsl2_underlyingL :: (OrdToDim1 s, PreDeltaSet2 s) => Lens (WithSuperfaceLookup2 s) s
wsl2_underlyingL = lens wsl2_underlying (const . addSuperfaceLookup2)


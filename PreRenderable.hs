{-# LANGUAGE RecordWildCards, TemplateHaskell, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module PreRenderable(
    -- * Reex
    module Math.Groups.S3,
    Vec2(..),Vec3(..),(&+),(&-),(&*),(*&),

    -- * Main
    TriangleLabel(..),
    defaultTriangleLabelUpDisplacement,
    Visibility(..),
    FaceName,mkFaceName,unFaceName,
    TriangleEmbedding(..),
    GeneralTriangleEmbedding(..),
    EdgeEmbedding(..),
    GeneralEdgeEmbedding(..),
    PreRenderable(..),
    Coords(..),
    mkPreRenderable,
    mkPreRenderableWithTriangleLabels,
    tet3d,
    SimplicialTriangleLabelAssoc(..),
    sTriangleLabelAssoc,
    triangleLabelsForSimplicial,
    pr_mapDs,
    pr_popDimension,
    pr_faceName,
    pr_setVisibility,
    pr_triangleLabel,
    pr_setTriangleEmbedding,
    pr_mbGeneralTriangleEmbedding,
    pr_coords,
    pr_edgeEmbedding,
    pr_triangleEmbedding,
    pr_visibility

    ) where

import Data.Vect.Double.Base hiding((*.),(.*),(.*.))
import DisjointUnion
import GHC.Generics
import Math.Groups.S3
import ShortShow
import Simplicial.AnySimplex
import Simplicial.DeltaSet2
import Simplicial.SimplicialComplex
import Tetrahedron.Vertex
import PreRenderable.TriangleLabel
import Control.Arrow
import Data.String
import MathUtil
import THUtil
import Util
import Control.Monad
import Control.Exception(assert)
import PrettyUtil(Pretty(..),prettyRecord,prettyFunction,prettyPrecFromShow)

class Coords t where
    transformCoords :: (Vec3 -> Vec3) -> t -> t

data Visibility = Visible | Invisible
    deriving Show

instance Pretty Visibility where prettyPrec = prettyPrecFromShow

newtype FaceName = FaceName { unFaceName :: String }
    deriving (Show,IsString,Pretty)

mkFaceName :: String -> FaceName
mkFaceName = FaceName

data PreRenderable s = PreRenderable {
    pr_ds :: s,
    -- | Overriden ('pr_coords') by the GeneralTriangleEmbedding in pr_triangleInfo, if it exists for at least one triangle containing the vertex
    pr_coords0 :: Vert s -> Vec3,
    pr_faceInfo :: AnySimplex2Of s -> (Visibility,FaceName), 
    pr_triangleInfo :: Tri s -> (Maybe TriangleLabel, Maybe GeneralTriangleEmbedding)
}
    deriving(Generic)

pr_mbGeneralTriangleEmbedding
  :: PreRenderable s -> Tri s -> Maybe GeneralTriangleEmbedding
pr_mbGeneralTriangleEmbedding = fmap snd . pr_triangleInfo

instance Coords (PreRenderable a) where
    transformCoords f _pr = _pr { pr_coords0 = f . pr_coords0 _pr }

instance 
    (   DisjointUnionable s1 s2 s
    
    ,   CoDisjointUnionable
                        (Vert s1) (Vert s2) (Vert s)
    ,   CoDisjointUnionable
                        (Ed s1) (Ed s2) (Ed s)
    ,   CoDisjointUnionable
                        (Tri s1) (Tri s2) (Tri s)
    ) =>
    DisjointUnionable (PreRenderable s1) (PreRenderable s2) (PreRenderable s) where

    disjointUnion = defaultDisjointUnion




mkPreRenderable
  :: (ShortShow (Vert s), ShortShow (Arc s), ShortShow (Tri s)) =>
     (Vert s -> Vec3) -> s -> PreRenderable s
mkPreRenderable = mkPreRenderableWithTriangleLabels (const Nothing)

mkPreRenderableWithTriangleLabels
  :: (ShortShow (Vert s), ShortShow (Arc s), ShortShow (Tri s)) =>

     (Tri s -> Maybe TriangleLabel)
     -> (Vert s -> Vec3) -> s -> PreRenderable s
mkPreRenderableWithTriangleLabels triangleLabels pr_coords_ pr_ds_ = 
    PreRenderable {
        pr_ds = pr_ds_,
        pr_coords0 = pr_coords_, 
        pr_triangleInfo = triangleLabels &&& const Nothing,
        pr_faceInfo = const Visible &&& (mkFaceName . foldAnySimplex2 shortShow shortShow shortShow)
    }

instance OneSkeletonable s => OneSkeletonable (PreRenderable s) where
    oneSkeleton _pr = _pr { pr_ds = oneSkeleton (pr_ds _pr) }


--type PreRenderableSimplicialComplex v = PreRenderable (OTuple v)

--tet3d :: PreRenderableSimplicialComplex Vertex
tet3d :: PreRenderable (SC3 Vertex)
tet3d = mkPreRenderable vertexDefaultCoords abstractTet 


pr_mapDs
  :: (Vert t ~ Vert s,
      Ed t ~ Ed s,
      Tri t ~ Tri s) =>
     (t -> s) -> PreRenderable t -> PreRenderable s
pr_mapDs f (PreRenderable a b c d) = PreRenderable (f a) b c d

pr_popDimension :: PreRenderable (SC3 v) -> PreRenderable (SC2 v)
pr_popDimension = pr_mapDs sccons_skeleton

pr_faceName :: PreRenderable s -> AnySimplex2Of s -> FaceName
pr_faceName = (fmap . fmap) snd pr_faceInfo

pr_setVisibility
  :: (AnySimplex2Of s -> Visibility)
     -> PreRenderable s -> PreRenderable s
pr_setVisibility f pr = pr { pr_faceInfo = f &&& pr_faceName pr } 

pr_triangleLabel :: PreRenderable s -> Tri s -> Maybe TriangleLabel
pr_triangleLabel = fmap fst . pr_triangleInfo

pr_setTriangleEmbedding
  :: (Tri s -> Maybe GeneralTriangleEmbedding)
     -> PreRenderable s -> PreRenderable s
pr_setTriangleEmbedding f pr = pr { pr_triangleInfo = pr_triangleLabel pr &&& f }

type Resolution = Int

data GeneralTriangleEmbedding = 
    GTE Resolution (Standard2Simplex -> Vec3)

data TriangleEmbedding = 
        FlatTriangle Vec3 Vec3 Vec3
    |   GeneralTriangle GeneralTriangleEmbedding

data GeneralEdgeEmbedding = GEE Resolution (UnitInterval -> Vec3)

data EdgeEmbedding =
        FlatEdge Vec3 Vec3
    |   GeneralEdge GeneralEdgeEmbedding

pr_edgeEmbedding
  :: (DeltaSet2 s,
      Pretty (Ed s),
      Pretty (Tri s)) =>
     PreRenderable s
     -> TrianglesContainingEdge_Cache (Ed s) (Tri s)
     -> Ed s
     -> EdgeEmbedding
pr_edgeEmbedding pr tcec e =
            case findJust (\(triangle,i) -> do 
                        emb <- pr_mbGeneralTriangleEmbedding pr triangle
                        return (triangle,i,emb))
                 . lookupTrianglesContainingEdge tcec
                 $ e of 

                 Nothing -> flatFace pr (uncurry FlatEdge) map2 faces10Ascending e 

                 -- edge is contained in some curved triangle
                 Just (triangle, i, GTE res emb) -> 
                    GeneralEdge
                        (GEE
                            res
                            (emb . pretransform))

                  where
                    pretransform = 
                        case i of
                             0 -> \t -> Vec2 (1-t) t 
                             1 -> \t -> Vec2 0 t 
                             2 -> \t -> Vec2 t 0    
                             _ -> error ("pr_edgeEmbedding/pretransform "++ $(showExps ['e,'triangle,'i]))

flatFace pr ctor mapN verticesAscending face =    
                                ctor
                            .   mapN (pr_coords0 pr)
                            .   verticesAscending (pr_ds pr)
                            $   face
                            

pr_triangleEmbedding :: DeltaSet2 s => PreRenderable s -> Tri s -> TriangleEmbedding
pr_triangleEmbedding pr t = 
    case pr_mbGeneralTriangleEmbedding pr t of
         Nothing -> flatFace pr (uncurry3 FlatTriangle) map3 faces20Ascending t

         Just emb -> GeneralTriangle emb

-- | See also: 'pr_coords0'
pr_coords
  :: (DeltaSet2 s,
      Pretty (Element (Eds s)),
      Pretty (Element (Tris s))) =>
     PreRenderable s
     -> TrianglesContainingEdge_Cache (Ed s) (Tri s)
     -> EdgesContainingVertex_Cache (Vert s) (Ed s)
     -> Vert s
     -> Vec3
pr_coords pr tcec ecvc v = 

    (
            findJust (\(e,i) -> 
                    case pr_edgeEmbedding pr tcec e of
                         FlatEdge {} -> Nothing
                         GeneralEdge (GEE _ emb) ->
                            Just (case i of
                                       0 -> emb 1
                                       1 -> emb 0
                                       _ -> assert False undefined))

                 . lookupEdgesContainingVertex ecvc
                 $ v 
    )
    `orElse`
    pr_coords0 pr v



instance (Pretty s, Pretty (Vert s), Pretty (Ed s), Pretty (Tri s)
            , Vertices s, Edges s, Triangles s) => 
    Pretty (PreRenderable s) where


    pretty PreRenderable{..} = 
        prettyRecord "PreRenderable"
            [ ("pr_ds",pretty pr_ds)
            , ("pr_coords0",prettyFunction pr_coords0 (vertices pr_ds))
            , ("pr_faceInfo",prettyFunction pr_faceInfo (anySimplex2s pr_ds))
            , ("pr_triangleInfo",prettyFunction 
                    (second (fmap (const "<<function>>")) . pr_triangleInfo) (triangles pr_ds))
            ]


pr_visibility :: PreRenderable s -> AnySimplex2Of s -> Visibility
pr_visibility = fmap fst . pr_faceInfo

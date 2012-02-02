{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, RecordWildCards, TemplateHaskell, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}
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
    pr_setEdVisibility,
    pr_setTriVisibility,
    pr_hide,
    pr_hideEds,
    pr_hideTris,
    pr_triangleLabel,
    pr_setTriangleEmbedding,
    pr_mbGeneralTriangleEmbedding,
    pr_coords,
    pr_edgeEmbedding,
    pr_triangleEmbedding,
    pr_visibility,
    pr_vertVisibility,
    pr_edVisibility,
    pr_triVisibility,
    pr_quad

    ) where

import Control.Arrow
import Control.Exception(assert)
import Control.Monad
import Data.AscTuples
import Data.String
import Data.Vect.Double.Base hiding((*.),(.*),(.*.))
import Data.Vect.Double.Interpolate
import DisjointUnion
import GHC.Generics
import Language.Haskell.TH.Lift
import Math.Groups.S3
import MathUtil
import OrphanInstances.Lift()
import PreRenderable.TriangleLabel
import PrettyUtil(Pretty(..),prettyRecord,prettyFunction,prettyPrecFromShow)
import ShortShow
import Simplicial.DeltaSet2
import Simplicial.SimplicialComplex
import THUtil
import Tetrahedron.Vertex
import Util
import Data.Lens.Template
import Data.Lens.Common

type Resolution = Int

data GeneralTriangleEmbedding = 
    GTE Resolution (Unit2SimplexPoint -> Vec3)

data TriangleEmbedding = 
        FlatTriangle Vec3 Vec3 Vec3
    |   GeneralTriangle GeneralTriangleEmbedding

data GeneralEdgeEmbedding = GEE Resolution (UnitIntervalPoint -> Vec3)

data EdgeEmbedding =
        FlatEdge Vec3 Vec3
    |   GeneralEdge GeneralEdgeEmbedding


class Coords t where
    transformCoords :: (Vec3 -> Vec3) -> t -> t

data Visibility = Visible | Invisible
    deriving (Eq,Ord,Show)

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
    pr_triangleInfo :: Tri s -> (Maybe TriangleLabel, Maybe GeneralTriangleEmbedding),
    pr_edgeInfo :: Ed s -> Maybe GeneralEdgeEmbedding
}
    deriving(Generic)

nameMakeLens ''PreRenderable (Just . (++"L"))


pr_quad
  :: Resolution -> (UnitSquare -> Vec3) -> PreRenderable (SC2 (Bool, Bool))
pr_quad reso f = 
    PreRenderable {
        pr_ds = ds,
        pr_coords0 = const (assert False undefined),
        pr_faceInfo = \asi -> ( if asi == edToAnySimplex2 _diag then Invisible else Visible
                              , mkFaceName $ shortShow asi),

        pr_triangleInfo = 
            \t -> 
                (Nothing,
                 
                    let pretransform =
                            if elemAsc3 ff t 
                                then id
                                else \(Vec2 u v) -> Vec2 (u+v) (1-v)

                    in (Just . GTE reso) (f . pretransform)
                    )

            
            
                ,

        pr_edgeInfo = const (assert False undefined)

    }

  where
    ff = (False,False)
    ft = (False,True)
    tf = (True,False)
    tt = (True,True)

    ds = fromTris [(ff,ft,tf),(ft,tf,tt)] 
    _diag = asc2 (ft,tf)


pr_mbGeneralTriangleEmbedding
  :: PreRenderable s -> Tri s -> Maybe GeneralTriangleEmbedding
pr_mbGeneralTriangleEmbedding = fmap snd . pr_triangleInfo

instance Coords (PreRenderable a) where
    transformCoords f _pr = _pr { pr_coords0 = f . pr_coords0 _pr }

instance 
    (   DisjointUnionable s1 s2 s inj1 inj2 ei
    
    ,   CoDisjointUnionable
                        (Vert s1) (Vert s2) (Vert s)
    ,   CoDisjointUnionable
                        (Ed s1) (Ed s2) (Ed s)
    ,   CoDisjointUnionable
                        (Tri s1) (Tri s2) (Tri s)
    ) =>
    DisjointUnionable (PreRenderable s1) (PreRenderable s2) (PreRenderable s) inj1 inj2 ei where

    disjointUnionWithInjs = fmap djZapUnits . defaultDisjointUnion




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
        pr_faceInfo = const Visible &&& (mkFaceName . shortShow),
        pr_edgeInfo = const Nothing
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
pr_mapDs f (PreRenderable a b c d e) = PreRenderable (f a) b c d e

pr_popDimension :: PreRenderable (SC3 v) -> PreRenderable (SC2 v)
pr_popDimension = pr_mapDs sccons_skeleton

pr_faceName :: PreRenderable s -> AnySimplex2Of s -> FaceName
pr_faceName = (fmap . fmap) snd pr_faceInfo

pr_visibilityL :: Lens (PreRenderable s) (AnySimplex2Of s -> Visibility)
pr_visibilityL = pr_faceInfoL >>> firstLens

pr_setVisibility :: (AnySimplex2Of s -> Visibility) -> PreRenderable s -> PreRenderable s
pr_setVisibility = setL pr_visibilityL

pr_triVisibilityL = pr_visibilityL >>> rightLens

pr_edVisibilityL :: Lens (PreRenderable s) (Ed s -> Visibility)
pr_edVisibilityL = pr_visibilityL >>> leftLens >>> rightLens

pr_vertVisibilityL = pr_visibilityL >>> leftLens >>> leftLens

pr_setEdVisibility
  :: (Ed s -> Visibility) -> PreRenderable s -> PreRenderable s
pr_setEdVisibility = setL pr_edVisibilityL

pr_setTriVisibility = setL pr_triVisibilityL

pr_vertVisibility = getL pr_vertVisibilityL 
pr_edVisibility = getL pr_edVisibilityL
pr_triVisibility = getL pr_triVisibilityL



pr_triangleLabel :: PreRenderable s -> Tri s -> Maybe TriangleLabel
pr_triangleLabel = fmap fst . pr_triangleInfo


pr_triangleEmbeddingL = pr_triangleInfoL >>> secondLens

pr_setTriangleEmbedding
  :: (Tri s -> Maybe GeneralTriangleEmbedding)
     -> PreRenderable s -> PreRenderable s
pr_setTriangleEmbedding = setL pr_triangleEmbeddingL


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

                 Nothing -> 
                    case pr_edgeInfo pr e of
                        Just gee -> GeneralEdge gee 
                        Nothing -> flatFace pr (uncurry FlatEdge) map2 faces10Ascending e 

                 -- edge is contained in some curved triangle
                 Just (_, i, GTE res emb) -> 
                    GeneralEdge
                        (GEE
                            res
                            (emb . pretransform))

                  where
                    pretransform = 
                        let (v0,v1) = faces10 Unit2Simplex (face21 Unit2Simplex Unit2Simplex i)
                        in \x -> interpolate x v1 v0

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
                            Just (emb (face10 UnitInterval UnitInterval i)))

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
pr_visibility = getL pr_visibilityL



-- instance Lift TriangleEmbedding where
--     lift (FlatTriangle a b c) = [| FlatTriangle a b c |]
-- 
-- instance Lift EdgeEmbedding where
--     lift (FlatEdge a b) = [| FlatEdge a b |]

-- | error instance
instance Lift GeneralTriangleEmbedding where
    lift _ = [| error "'lift' not supported for GeneralTriangleEmbedding" |]

-- | error instance
instance Lift GeneralEdgeEmbedding where
    lift _ = [| error "'lift' not supported for GeneralEdgeEmbedding" |]

instance Lift FaceName where
    lift (unFaceName -> x) = [| mkFaceName x |] 

deriveLiftMany [''Visibility,''TriangleEmbedding,''EdgeEmbedding]

instance (DeltaSet2 s, Lift s, Ord (Vert s), Ord (Ed s), Ord (Tri s)
            , Lift (Vert s), Lift (Ed s), Lift (Tri s)) => 

    Lift (PreRenderable s) where

    lift PreRenderable{..} = 
        [| PreRenderable {
                pr_ds = pr_ds,
                pr_coords0 = $(liftFunction (vertexList pr_ds) pr_coords0),
                pr_faceInfo = $(liftFunction (anySimplex2s pr_ds) pr_faceInfo),
                pr_triangleInfo = $(liftFunction (triangleList pr_ds) pr_triangleInfo),
                pr_edgeInfo = $(liftFunction (edgeList pr_ds) pr_edgeInfo)
        } |]


-- pr_ghide
--   :: ((t1 -> Visibility) -> PreRenderable s -> t)
--      -> (t1 -> AnySimplex2Of s) -> (t1 -> Bool) -> PreRenderable s -> t
-- pr_ghide _setVisibility f p pr =
--     _setVisibility (\asi -> visibleIf (not (p asi)) * pr_visibility pr (f asi)) pr

pr_ghide :: Lens a (t -> Visibility) -> (t -> Bool) -> a -> a
pr_ghide _lens p = modL _lens (\oldVi -> \x -> visibleIf (not (p x)) * oldVi x) 

pr_hide
  :: (AnySimplex2Of s -> Bool) -> PreRenderable s -> PreRenderable s
pr_hide = pr_ghide pr_visibilityL


--pr_hideVerts = pr_ghide pr_setVertVisibility vertToAnySimplex2

pr_hideEds :: (Ed s -> Bool) -> PreRenderable s -> PreRenderable s
pr_hideEds = pr_ghide pr_edVisibilityL

pr_hideTris
  :: (Tri s -> Bool) -> PreRenderable s -> PreRenderable s
pr_hideTris = pr_ghide pr_triVisibilityL

visibleIf b = if b then Visible else Invisible

instance Num Visibility where
    (*) Visible Visible = Visible
    (*) _ _ = Invisible

    (+) Invisible Invisible = Invisible
    (+) _ _ = Visible

    negate Visible = Invisible
    negate Invisible = Visible

    abs = assert False undefined
    signum = assert False undefined
    fromInteger = assert False undefined


isRegardedAsSimplexByDisjointUnionDeriving [t| (Bool,Bool) |]

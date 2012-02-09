{-# LANGUAGE CPP, TemplateHaskell, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, DeriveGeneric, ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, GADTs, TypeFamilies, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module Blender.Blenderable where

import ConcreteNormal.PreRenderable
import Control.Category((>>>))
import Data.Function
import Data.Lens.Common
import Data.Lens.Template
import Data.List
import GHC.Generics
import Prelude hiding(catch)
import PrettyUtil
import THUtil
import ToPython
import Util
import Language.Haskell.TH.Lift
import qualified Data.Foldable as Fold
import Data.Foldable(Foldable)
import Blender.Types
import Data.Maybe

bpy_ops ::  String -> Python ()
bpy_ops x = py "ops" <.> x
bpy_dat ::  String -> Python ()
bpy_dat x = py "data" <.> x

bpy_props ::  Python ()
bpy_props = py "props"
bpy_types ::  String -> Python ()
bpy_types x = py "types" <.> x




triangleUVLayerName :: UVLayerName
triangleUVLayerName = "Triangle UV from hs"
                                           
data BaFaceInfo = BaFaceInfo {
    faceMat :: Material,
    bfi_groups :: [BlenderGroup],
    bfi_labelMat :: Maybe Material,
    bfi_helpLineMat :: Maybe Material
}

nameMakeLens ''BaFaceInfo (Just . (++"L"))



data Blenderable s = Blenderable { 
    ba_pr :: PreRenderable s,
    ba_faceInfo :: AnySimplex2Of s -> BaFaceInfo,
    ba_vertexThickness :: Vert s -> Double,
    ba_edgeThickness :: Arc s -> Double
}
    deriving (Generic)


nameMakeLens ''Blenderable (Just . (++"L"))

ba_pr_triangleInfo
  :: Blenderable s
     -> Tri s -> (Maybe TriangleLabel, Maybe GeneralTriangleEmbedding)
ba_pr_triangleInfo = pr_triangleInfo . ba_pr


ba_triangleInfoL
  :: Lens (Blenderable s) (Tri s -> BaFaceInfo)
ba_triangleInfoL = ba_faceInfoL >>> rightLens 




instance Coords (Blenderable s) where
    transformCoords f = modL ba_prL (transformCoords f)


-- instance (BlenderLabel s, BlenderLabel b) => BlenderLabel (DisjointUnion s b) where
--     blenderLabel (DisjointUnion s b) n = blenderLabel s n ||| blenderLabel b n 




data Scene s = Scene {
    scene_blenderable :: Blenderable s,
    scene_worldProps :: Props,
    scene_cams :: [Cam],
    scene_setLongLabelCustomProperties :: Bool,
    scene_initialSelection :: Maybe (AnySimplex2Of s),
    scene_render :: RenderSettings
}

nameMakeLens ''Scene (Just . (++"L"))



                        
setInitialSelection :: AnySimplex2Of s -> Scene s -> Scene s
setInitialSelection = setL scene_initialSelectionL . Just














--     where
--         sh :: forall n. Nat n => n -> s n -> String
--         sh _ = showN 



-- | Points cam at positive y dir
defaultScene0 :: Blenderable s -> Scene s
defaultScene0 s = Scene {
    scene_blenderable = s,
    scene_worldProps = defaultWorldProps,
    scene_cams = [defaultCam],
    scene_setLongLabelCustomProperties = False,
    scene_initialSelection = Nothing,
    scene_render = defaultRenderSettings
}

defaultCam :: Cam
defaultCam = Cam (Vec3 0.66 (-2.3) 0.52) (eulerAnglesXYZ (pi/2) 0 0) defaultFOV


defaultWorldProps :: Props
defaultWorldProps = ["use_sky_blend" & False,
                     "use_sky_real" & False,
                     "horizon_color" & ((1,1,1)::(Int,Int,Int))
                     ]





instance 
    (   DisjointUnionable s1 s2 s inj1 inj2 ei
    
    ,   CoDisjointUnionable
                        (Vert s1) (Vert s2) (Vert s)
    ,   CoDisjointUnionable
                        (Ed s1) (Ed s2) (Ed s)
    ,   CoDisjointUnionable
                        (Tri s1) (Tri s2) (Tri s)
    ) =>
    DisjointUnionable (Blenderable s1) (Blenderable s2) (Blenderable s) 
        inj1 inj2 ei
    
        where



    disjointUnionWithInjs = fmap djZapUnits . defaultDisjointUnion






--normalSurface' s = normalSurface (addCoordFunc id (const Nothing) s)

instance (Pretty s, Pretty (Vert s), Pretty (Ed s), Pretty (Tri s)
            , Vertices s, Edges s, Triangles s) => 
                    Pretty (Blenderable s) where

    pretty Blenderable{..} = prettyRecord "Blenderable"
        [("ba_pr",pretty ba_pr)
--         ("ba_coords",prettyFunction ba_coords (vertices ba_ds))
        ]

instance (Pretty s, Pretty (Vert s), Pretty (Ed s), Pretty (Tri s)
            , Vertices s, Edges s, Triangles s) => 
                    Show (Blenderable s) where

    showsPrec = prettyShowsPrec

deriving instance (Show (AnySimplex2Of s), Show (Blenderable s)) => Show (Scene s)

setCams :: [Cam] -> Scene s -> Scene s
setCams = setL scene_camsL

setCam :: Cam -> Scene s -> Scene s
setCam = setCams . return

setRS :: RenderSettings -> Scene s -> Scene s
setRS = setL scene_renderL


-- | = @uncurry 'fromSpqwcAndIntegerNormalSurface'@
-- instance (Ord v, ShortShow v, StandardCoords s Integer, Pretty v, Show v) => 
--     ToBlenderable (SPQWithCoords v, s) (DJ (SC2 v) (SC2 (Corn v))) where
-- 
--     toBlenderable = uncurry fromSpqwcAndIntegerNormalSurface

ba_triangleLabel :: Blenderable s -> Tri s -> Maybe TriangleLabel
ba_triangleLabel = fmap fst . ba_pr_triangleInfo 

ba_triangleEmbedding :: PreDeltaSet2 s => Blenderable s -> Tri s -> TriangleEmbedding
ba_triangleEmbedding = pr_triangleEmbedding . ba_pr

ba_edgeEmbedding
  :: (Show (Element (Eds s)),
      Show (Element (Tris s)),
      Show (Vert s),
      PreDeltaSet2 s) =>
     Blenderable s
     -> TrianglesContainingEdge_Cache (Ed s) (Tri s)
     -> Ed s
     -> EdgeEmbedding
ba_edgeEmbedding = pr_edgeEmbedding . ba_pr

ba_coords
  :: (Show (Element (Eds s)),
      Show (Element (Tris s)),
      Show (Vert s),
      PreDeltaSet2 s) =>
     Blenderable s
     -> TrianglesContainingEdge_Cache (Ed s) (Tri s)
     -> EdgesContainingVertex_Cache (Vert s) (Ed s)
     -> Vert s
     -> Vec3
ba_coords = pr_coords . ba_pr

ba_faceName :: Blenderable s -> AnySimplex2Of s -> FaceName
ba_faceName = pr_faceName . ba_pr

ba_visibility :: Blenderable s -> AnySimplex2Of s -> Visibility
ba_visibility = pr_visibility . ba_pr

ba_faceMat :: Blenderable s -> AnySimplex2Of s -> Material
ba_faceMat = fmap faceMat . ba_faceInfo



#ifdef DERIVE_LIFT
deriveLiftMany [''TransparencySettings,''Material,''BaFaceInfo]

instance (PreDeltaSet2 s, Lift s, Ord (Vert s), Ord (Ed s), Ord (Tri s)
            , Lift (Vert s), Lift (Ed s), Lift (Tri s)) => 

            Lift (Blenderable s) where

     lift Blenderable{..} = 
        [| Blenderable {
                ba_pr = ba_pr,
                ba_faceInfo = $(liftFunction (anySimplex2s ds) ba_faceInfo),
                ba_vertexThickness = $(liftFunction (vertexList ds) ba_vertexThickness),
                ba_edgeThickness = $(liftFunction (edgeList ds) ba_edgeThickness)
        } |]


        where ds = pr_ds ba_pr
#endif

ba_ds :: Blenderable s -> s
ba_ds = pr_ds . ba_pr


ba_allFaceInfos
  :: (Vertices s, Triangles s, Edges s) =>
     Blenderable s -> [BaFaceInfo]
ba_allFaceInfos ba = map (ba_faceInfo ba) (anySimplex2s (ba_ds ba))




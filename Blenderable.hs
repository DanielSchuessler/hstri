{-# LANGUAGE TemplateHaskell, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, DeriveGeneric, ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, GADTs, TypeFamilies, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Blenderable where


import Data.Function
import Data.List
import Data.Vect.Double
import DisjointUnion
import HomogenousTuples
import Prelude hiding(catch,mapM_,sequence_) 
import ToPython
import PreRenderable
import GHC.Generics
import SimplicialPartialQuotient
import Simplicial.SimplicialComplex
import Simplicial.DeltaSet2
import ConcreteNormal.PreRenderable
import NormalEverything
import PrettyUtil
import ShortShow
import Util
import THUtil
import Data.AscTuples
import Language.Haskell.TH.Lift

data Blenderable s = Blenderable { 
    ba_pr :: PreRenderable s,
    ba_faceInfo :: AnySimplex2Of s -> BaFaceInfo,
    ba_vertexThickness :: Vert s -> Double,
    ba_edgeThickness :: Arc s -> Double,
    ba_materials :: [Material]
}
    deriving (Generic)


ba_triangleInfo
  :: Blenderable s
     -> Tri s -> (Maybe TriangleLabel, Maybe GeneralTriangleEmbedding)
ba_triangleInfo = pr_triangleInfo . ba_pr


instance Coords (Blenderable s) where
    transformCoords f b = b { ba_pr = transformCoords f (ba_pr b) }

data BaFaceInfo = BaFaceInfo {
        faceMat :: Material
}

-- instance (BlenderLabel s, BlenderLabel b) => BlenderLabel (DisjointUnion s b) where
--     blenderLabel (DisjointUnion s b) n = blenderLabel s n ||| blenderLabel b n 

type MatName = String


type EulerAnglesXYZ = Vec3

eulerAnglesXYZ :: Double -> Double -> Double -> EulerAnglesXYZ
eulerAnglesXYZ = Vec3

data Scene s = Scene {
    scene_blenderable :: Blenderable s,
    scene_worldProps :: Props,
    scene_cams :: [Cam]
}



data Cam = Cam {
    cam_pos :: Vec3,
    -- | XYZ eulers
    cam_eulers :: EulerAnglesXYZ,
    cam_FOV :: Double
}
    deriving(Show)

defaultFOV :: Double
defaultFOV = 0.8575560591178853

setCams :: [Cam] -> Scene s -> Scene s
setCams scene_cams s = s { scene_cams }
                        
data Material = Material {
    ma_name :: MatName,
    ma_props :: Props
}
    deriving Show

diffuseColor :: Triple Double -> MatProp
diffuseColor rgb = "diffuse_color" & rgb


type MatProp = (String,Python ())
type Props = [MatProp]


specular :: 
       Int -- ^ [1,511]
    -> Double -- ^ [0,1]
    -> Props
specular hardness intensity = ["specular_hardness" & hardness,
                               "specular_intensity" & intensity]

transparency :: Double -> Double -> Double -> Props
transparency alpha spec_alpha fresnel =
                [
                "use_transparency" & True,
                "transparency_method" & str "RAYTRACE",
                -- "transparency_method" & "'Z_TRANSPARENCY'",
                "alpha" & (alpha::Double),
                "specular_alpha" & (spec_alpha::Double),
                "translucency" & (1-alpha),
                "raytrace_transparency.fresnel_factor" & (1::Int),
                "raytrace_transparency.fresnel" & (fresnel::Double),
                "raytrace_transparency.depth" & (15::Int)
                ]


data Style = Style {
    mat0, mat1, mat2 :: Material,
    vertexThickness :: Double
}

mkBlenderable
  :: forall s. 
        Style
     -> PreRenderable s
     -> Blenderable s
mkBlenderable Style{..} pr_ = Blenderable { 
    ba_pr = pr_,

    ba_faceInfo = \asi ->
        BaFaceInfo {
            faceMat = foldAnySimplex2 (const mat0) (const mat1) (const mat2) asi
        },

    ba_vertexThickness = const vertexThickness,
    ba_edgeThickness = const (edgeThicknessFactor*vertexThickness),

    ba_materials = [mat0,mat1,mat2]
}





pseudomanifoldStyle :: Style
pseudomanifoldStyle = Style 
    (Material "pmMat0" (let r=0.3 in diffuseColor (r, r, r):[]))
    (Material "pmMat1" (diffuseColor (0.7, 0.7, 0.8):specular 100 0.8))
    (Material "pmMat2" (diffuseColor (0.7, 0.7, 0.8):specular 100 0.8++transparency 0.25 0.3 1))
    pmVertThickness  


pmVertThickness ::  Double
pmVertThickness = 0.04
edgeThicknessFactor ::  Double
edgeThicknessFactor = 0.4
nsurfVertThickness ::  Double
nsurfVertThickness = 0.03






--     where
--         sh :: forall n. Nat n => n -> s n -> String
--         sh _ = showN 




mkNormalSurfaceStyle
  :: [Char]
     -> Triple Double -> Triple Double -> Triple Double -> Style
mkNormalSurfaceStyle suf col0 col1 col2 = Style 
    (Material ("nsurfMat0"++suf) (diffuseColor col0 :[]))
    (Material ("nsurfMat1"++suf) (diffuseColor col1:specular 100 0.8))
    (Material ("nsurfMat2"++suf) (diffuseColor col2:specular 100 0.8++transparency 0.55 0.7 1))
    nsurfVertThickness

normalSurfaceStyle :: Style
normalSurfaceStyle = mkNormalSurfaceStyle ""
 (0, 0.4, 1)
 (0, 0.2, 1)
 (0, 0, 1)

normalSurfaceStyleB :: Style
normalSurfaceStyleB = mkNormalSurfaceStyle "B"
 (0, 1,0)
 (0, 0.8,0)
 (0, 0.6,0)

-- | Points cam at positive y dir
defaultScene :: ToBlenderable x s => x -> Scene s
defaultScene s = Scene (toBlenderable s) defaultWorldProps [defaultCam]

defaultCam :: Cam
defaultCam = Cam (Vec3 0.66 (-2.3) 0.52) (eulerAnglesXYZ (pi/2) 0 0) defaultFOV

(&) :: ToPython s => t -> s -> (t, Python ())
x & y = (x, toPython y)

defaultWorldProps :: Props
defaultWorldProps = ["use_sky_blend" & False,
                     "use_sky_real" & False,
                     "horizon_color" & ((1,1,1)::(Int,Int,Int))
                     ]



instance DisjointUnionable [Material] [Material] [Material] () () () where
    disjointUnionWithInjs ma ma' =

        DisjointUnion 
            (nubBy ((==) `on` ma_name) (ma++ma'))
            () () ()


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

deriving instance (Show (Blenderable s)) => Show (Scene s)

-- * Conversion

class ToBlenderable x s | x -> s where
    toBlenderable :: x -> Blenderable s

instance ToBlenderable (Blenderable s) s where
    toBlenderable = id

fromSpqwc
  :: (Ord v, ShortShow v,Pretty v,Show v) =>
     SPQWithCoords v
     -> Blenderable (SC2 v)
fromSpqwc = mkBlenderable pseudomanifoldStyle . pr_popDimension . toPreRenderable

-- | = 'fromSpqwc'
instance (Ord v, ShortShow v, Pretty v, Show v) => ToBlenderable (SPQWithCoords v) (SC2 v) where
    toBlenderable = fromSpqwc

fromNormalSurface
  :: (Integral i, Ord v, Pretty i, ShortShow v, StandardCoords s i, Show v) =>
     SPQWithCoords v -> s -> Blenderable (SC2 (Corn v))
fromNormalSurface spqwc stc = mkBlenderable normalSurfaceStyle (normalSurfaceToPreRenderable spqwc stc) 

fromIntegerNormalSurface
  :: (Ord v, ShortShow v, StandardCoords s Integer, Show v) =>
     SPQWithCoords v -> s -> Blenderable (SC2 (Corn v))
fromIntegerNormalSurface = fromNormalSurface


fromSpqwcAndIntegerNormalSurface
  :: (Ord v,
      Show v,
      Pretty v,
      ShortShow v,
      StandardCoords s Integer) =>
     SPQWithCoords v
     -> s
     -> Blenderable
          (DJSCons
             (Asc3 v)
             (Asc3 (Corn v))
             (DJSCons (Asc2 v) (Asc2 (Corn v)) (DJSCons v (Corn v) SCMinus1)))
fromSpqwcAndIntegerNormalSurface spqwc s = 
    fromSpqwc spqwc 
    `disjointUnion`
    fromIntegerNormalSurface spqwc s 


-- | = @uncurry 'fromSpqwcAndIntegerNormalSurface'@
-- instance (Ord v, ShortShow v, StandardCoords s Integer, Pretty v, Show v) => 
--     ToBlenderable (SPQWithCoords v, s) (DJ (SC2 v) (SC2 (Corn v))) where
-- 
--     toBlenderable = uncurry fromSpqwcAndIntegerNormalSurface

ba_triangleLabel :: Blenderable s -> Tri s -> Maybe TriangleLabel
ba_triangleLabel = fmap fst . ba_triangleInfo 

ba_triangleEmbedding :: DeltaSet2 s => Blenderable s -> Tri s -> TriangleEmbedding
ba_triangleEmbedding = pr_triangleEmbedding . ba_pr

ba_edgeEmbedding
  :: (Pretty (Element (Eds s)),
      Pretty (Element (Tris s)),
      DeltaSet2 s) =>
     Blenderable s
     -> TrianglesContainingEdge_Cache (Ed s) (Tri s)
     -> Ed s
     -> EdgeEmbedding
ba_edgeEmbedding = pr_edgeEmbedding . ba_pr

ba_coords
  :: (Pretty (Element (Eds s)),
      Pretty (Element (Tris s)),
      DeltaSet2 s) =>
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

-- | Read the result of printing @(cam.location,cam.rotation_euler,cam.data.angle)@ in Blender-python
readCam :: String -> Cam
readCam s = 
    case parseFloatLiterals s of
         [a,b,c,d,e,f,g] -> Cam (Vec3 a b c) (Vec3 d e f) g 
         r -> error ("readCam: no parse "++ $(showExps ['s,'r]))


deriveLiftMany [''Material,''BaFaceInfo]

instance (DeltaSet2 s, Lift s, Ord (Vert s), Ord (Ed s), Ord (Tri s)
            , Lift (Vert s), Lift (Ed s), Lift (Tri s)) => 

            Lift (Blenderable s) where

     lift Blenderable{..} = 
        [| Blenderable {
                ba_pr = ba_pr,
                ba_faceInfo = $(liftFunction (anySimplex2s ds) ba_faceInfo),
                ba_vertexThickness = $(liftFunction (vertexList ds) ba_vertexThickness),
                ba_edgeThickness = $(liftFunction (edgeList ds) ba_edgeThickness),
                ba_materials = ba_materials
        } |]


        where ds = pr_ds ba_pr

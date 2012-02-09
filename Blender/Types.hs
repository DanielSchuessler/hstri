{-# LANGUAGE CPP, TemplateHaskell, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, DeriveGeneric, ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, GADTs, TypeFamilies, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Blender.Types where

import ConcreteNormal.PreRenderable
import Data.Function
import Data.Lens.Template
import Data.List
import Prelude hiding(catch)
import THUtil
import ToPython
import Util
import qualified Data.Vector as V

(&) :: ToPython s => t -> s -> (t, Python ())
x & y = (x, toPython y)

type TextureName = String
type MatName = String
type UVLayerName = String

type BlenderPropAssoc = (String,Python ())
type Props = [BlenderPropAssoc]

data TransparencySettings = Trans {
    _alpha :: Double,
    _spec_alpha :: Double,
    _fresnel :: Double
}
    deriving Show

data BlenderImage = BImg {
    bimg_name :: String,
    bimg_filepath :: FilePath
}
    deriving Show

-- | NOTE: Textures in the same scene must have a unique name
data Texture = ImgTex {
    tex_name :: TextureName,
    imgtex_img :: BlenderImage
}
    deriving Show

data MaterialTextureSlot = MTS {
    mts_uv_layer :: UVLayerName,
    mts_props :: Props,
    mts_tex :: Texture
}
    deriving Show

-- | NOTE: Materials in the same scene must have a unique name
data Material = Material {
    ma_name :: MatName,
    ma_props :: Props,
    ma_transparency :: Maybe TransparencySettings,
    ma_textureSlots :: [MaterialTextureSlot]
}
    deriving Show

basicMaterial :: MatName -> Props -> Material
basicMaterial name props = Material name props Nothing []

nameMakeLens ''Material (Just . (++"L"))

type BlenderGroupName = String

newtype BlenderGroup = BlenderGroup { bgrp_name :: BlenderGroupName }
    deriving(Eq,Ord,Show)


type EulerAnglesXYZ = Vec3

eulerAnglesXYZ :: Double -> Double -> Double -> EulerAnglesXYZ
eulerAnglesXYZ = Vec3

data Cam = Cam {
    cam_pos :: Vec3,
    -- | XYZ eulers
    cam_eulers :: EulerAnglesXYZ,
    cam_FOV :: Double
}
    deriving(Show)

defaultFOV :: Double
defaultFOV = 0.8575560591178853


diffuseColor :: Triple Double -> BlenderPropAssoc
diffuseColor rgb = "diffuse_color" & rgb




specular :: 
       Int -- ^ [1,511]
    -> Double -- ^ [0,1]
    -> Props
specular hardness intensity = ["specular_hardness" & hardness,
                               "specular_intensity" & intensity]

transparency :: TransparencySettings -> [([Char], Python ())]
transparency Trans{..} =
                [
                "use_transparency" & True,
                "transparency_method" & str "RAYTRACE",
                -- "transparency_method" & "'Z_TRANSPARENCY'",
                "alpha" & (_alpha),
                "specular_alpha" & (_spec_alpha),
                "translucency" & (1-_alpha),
                "raytrace_transparency.fresnel_factor" & (1::Int),
                "raytrace_transparency.fresnel" & (_fresnel),
                "raytrace_transparency.depth" & (15::Int)
                ]

-- | Read the result of printing @(cam.location,cam.rotation_euler,cam.data.angle)@ in Blender-python
readCam :: String -> Cam
readCam s = 
    case parseFloatLiterals s of
         [a,b,c,d,e,f,g] -> Cam (Vec3 a b c) (Vec3 d e f) g 
         r -> error ("readCam: no parse "++ $(showExps ['s,'r]))


-- instance DisjointUnionable [Material] [Material] [Material] () () () where
--     disjointUnionWithInjs ma ma' =
-- 
--         DisjointUnion 
--             (nubBy ((==) `on` ma_name) (ma++ma'))
--             () () ()
-- 


data Mesh = Mesh {
        meshVertices :: [MeshVertex],
        meshFaces :: [Triple MeshVertexIndex],
        meshSmooth :: Bool,
        uv_textures :: [MeshUVLayer],
        meshMats :: [Material]
    }

type UV = Vec2

-- | Aka @MeshTextureFaceLayer@
data MeshUVLayer = MeshUVLayer UVLayerName [Triple UV] 

type MeshVertexIndex = Int
type MeshVertex = Vec3

data BlenderCurve = BlenderCurve {
    curve_name :: String,
    bevel_depth :: Double,
    -- | 0 to 32
    bevel_resolution :: Int,
    curve_splines :: [BlenderSpline],
    -- | At least 1. Apparently this is just for the preview, not for rendering.
    curve_resolution_u :: Int,
    curve_mats :: [Material]
}

data BlenderSpline = Nurbs {
    spline_points :: V.Vector SplinePoint,
    use_endpoint_u :: Bool,
    -- | \"Nurbs order in the U direction (For splines and surfaces), Higher values let points influence a greater area\"
    order_u :: Int,
    use_cyclic_u :: Bool,
    spline_resolution_u :: Int
}

type SplinePoint = Vec3


-- data BlenderObject d = BObj {
--     bobj_name :: String,
--     bobj_data :: d
-- }
-- 
-- 

data TextCurve = TextCurve {
    textCurve_name :: String,
    textCurve_text :: String,
    textCurve_mats :: [Material],
    -- | Thickness
    textCurve_extrude :: Double
}

data RenderSettings = RS {
    render_resolution_x :: Int, 
    render_resolution_y :: Int
}
   deriving Show
    
defaultRenderSettings :: RenderSettings
defaultRenderSettings = RS {
    render_resolution_x = 1920,
    render_resolution_y = 1080
}                         
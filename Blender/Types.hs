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
import Control.Monad
import Data.Vect.Double.Base(Vec3(..))
import Data.Vect.Double.Base(Proj4)
import Data.Lens.Common
import Control.Category((>>>))
import MathUtil

type BlenderUnits = Double
type TextureName = String
type MatName = String
type UVLayerName = String
type BlenderPropAssoc = (String,Python ())
type Props = [BlenderPropAssoc]
type BlenderEnergy = Double

(&) :: ToPython s => t -> s -> (t, Python ())
x & y = (x, toPython y)


data TransparencySettings = Trans {
    _alpha :: Double,
    _spec_alpha :: Double,
    _fresnel :: Double,
    _fresnel_factor :: Double,
    _translucency :: Double
}
    deriving Show

nameMakeLens ''TransparencySettings (Just . (++"L") . drop 1)

defaultTrans :: Double -- ^ alpha 
    -> TransparencySettings
defaultTrans _alpha = Trans {_alpha, _spec_alpha = _alpha, _fresnel = 1, _fresnel_factor = 1.25, _translucency=1-_alpha }

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
    ma_textureSlots :: [MaterialTextureSlot],

 -- | [1,511]
    ma_specular_hardness :: Int, 
 -- | [0,1]
    ma_specular_intensity :: Double,

    ma_diffuse_color :: Triple Double,

    ma_ambient :: Double
}
    deriving Show

nameMakeLens ''Material (Just . (++"L"))

basicMaterial :: MatName -> Triple Double -> Material
basicMaterial ma_name ma_diffuse_color = Material {
    ma_name,
    ma_props = [],
    ma_specular_hardness = 50,
    ma_specular_intensity = 0.5,
    ma_transparency = Nothing,
    ma_textureSlots = [],
    ma_diffuse_color,
    ma_ambient = 0
}



type BlenderGroupName = String

newtype BlenderGroup = BlenderGroup { bgrp_name :: BlenderGroupName }
    deriving(Eq,Ord,Show)


data EulerAnglesXYZ = EulerAnglesXYZ { euler_x,euler_y,euler_z :: Double }
    deriving (Show)

nameMakeLens ''EulerAnglesXYZ (Just . (++"L"))

eulerAnglesXYZ :: Double -> Double -> Double -> EulerAnglesXYZ
eulerAnglesXYZ = EulerAnglesXYZ

data Cam = Cam {
    cam_pos :: Vec3,
    -- | XYZ eulers
    cam_eulers :: EulerAnglesXYZ,
    cam_FOV :: Double
}
    deriving(Show)

nameMakeLens ''Cam (Just . (++"L"))

defaultFOV :: Double
defaultFOV = 0.8575560591178853

cam_normL :: Lens Cam Double
cam_normL = cam_posL >>> normL

cam_norm :: Cam -> Double
cam_norm = getL cam_normL

cam_withNormOf :: Cam -> Cam -> Cam
cam_withNormOf cam1 cam2 = setL cam_normL (cam_norm cam2) cam1




-- | Read the result of printing @(cam.location,cam.rotation_euler,cam.data.angle)@ in Blender-python
readCam :: String -> Cam
readCam s = 
    case parseFloatLiterals s of
         [a,b,c,d,e,f,g] -> Cam (Vec3 a b c) (eulerAnglesXYZ d e f) g 
         r -> error ("readCam: no parse "++ $(showExps ['s,'r]))


-- instance DisjointUnionable [Material] [Material] [Material] () () () where
--     disjointUnionWithInjs ma ma' =
-- 
--         DisjointUnion 
--             (nubBy ((==) `on` ma_name) (ma++ma'))
--             () () ()


data BlenderCurveBase = BlenderCurveBase {
    curve_name :: String,
    -- | At least 1. Apparently this is just for the preview, not for rendering.
    curve_resolution_u :: Int,
    curve_mats :: [Material]
}

data BlenderCurve = BlenderCurve {
    curve_base :: BlenderCurveBase,
    bevel_depth :: Double,
    -- | 0 to 32
    bevel_resolution :: Int,
    curve_splines :: [BlenderSpline]
}

data TextCurve = TextCurve {
    textCurve_base :: BlenderCurveBase,
    textCurve_text :: String,
    -- | Thickness
    textCurve_extrude :: Double
}

data BlenderSurface = BlenderSurface {
    surface_base :: BlenderCurveBase,
    surface_spline :: BlenderSpline2D
}

-- | Options applicable to each dimension of a spline
data SplineDimOpts = SplineDimOpts {
    use_endpoint :: Bool,
    -- | \"Nurbs order [...] (For splines and surfaces), Higher values let points influence a greater area\"
    order :: Int,
    use_cyclic :: Bool,
    spline_resolution :: Int
}

defaultSplineDimOpts :: SplineDimOpts
defaultSplineDimOpts = SplineDimOpts {
    use_endpoint = False,
    use_cyclic = False,
    order = 4,
    spline_resolution = 12
}


data BlenderSpline = Nurbs {
    spline_points :: V.Vector SplinePoint,
    spline_dimOpts :: SplineDimOpts
}

data BlenderSpline2D = Nurbs2D {
    -- | v-major
    spline2d_points :: V.Vector SplinePoint,
    spline2d_u :: SplineDimOpts,
    spline2d_v :: SplineDimOpts,
    spline2d_point_count_u :: Int
}

type SplinePoint = Vec3


-- data BlenderObject d = BObj {
--     bobj_name :: String,
--     bobj_data :: d
-- }
-- 
-- 


data RenderSettings = RS {
    render_resolution_x :: Int, 
    render_resolution_y :: Int,
    render_filepath :: Maybe FilePath
}
   deriving Show
    
nameMakeLens ''RenderSettings (Just . (++"L"))

defaultRenderSettings :: RenderSettings
defaultRenderSettings = RS {
    render_resolution_x = 1920,
    render_resolution_y = 1080,
    render_filepath = Nothing
}                         

data BlenderLamp = Sun {
    lamp_name :: String
}
    deriving Show

nameMakeLens ''BlenderLamp (Just . (++"L"))

data BlenderTransform = LocAndEulers {
    locAndEulers_loc :: Vec3,
    locAndEulers_eulers :: EulerAnglesXYZ
}
    |
    BProj4 Proj4 -- ^ This matrix is on the right, vectors are on the left. Generally works with the conventions of the /vect/ package.

    deriving Show

nameMakeLens ''BlenderTransform (Just . (++"L"))

btransLocOnly :: Vec3 -> BlenderTransform
btransLocOnly loc = LocAndEulers loc (eulerAnglesXYZ 0 0 0) 

data BlenderObject a = BObj {
    bobj_name :: String, 
    bobj_data :: a,
    bobj_transform :: BlenderTransform
}
    deriving Show

defaultBObj :: String -> a -> BlenderTransform -> BlenderObject a
defaultBObj = BObj

nameMakeLens ''BlenderObject (Just . (++"L"))

type LampObj = BlenderObject BlenderLamp


mkSunObj :: EulerAnglesXYZ -> BlenderObject BlenderLamp
mkSunObj eulers = 
    defaultBObj 
        (name ++ "Obj")
        Sun { lamp_name = name }
        LocAndEulers {
            locAndEulers_loc = (Vec3 (-5) (-10) 8), -- doesn't really matter for sun lamps
            locAndEulers_eulers = eulers
        }

    where
        name = "TheSun"

oldDefaultLamp :: LampObj
oldDefaultLamp = mkSunObj (eulerAnglesXYZ (5*pi/12) 0 (-pi/6))

defaultSun :: BlenderObject BlenderLamp
defaultSun = mkSunObj (eulerAnglesXYZ (0.25*pi) 0 (pi/6))

-- | = 'defaultSun'
defaultLamp :: LampObj
defaultLamp = defaultSun



ma_globalVarName :: Material -> [Char]
ma_globalVarName = ("material_"++) . ma_name

ma_globalVar :: Material -> PythonVar
ma_globalVar = pyv . ma_globalVarName

appendMaterialsStmt
  :: PythonVar -> [Material] -> Python ()
appendMaterialsStmt var mats = 
    forM_ mats (\m -> methodCall1 (var <.> "materials") "append" (ma_globalVar m))


data BlenderCamData = BCamD {
    bcamd_name :: String,
    bcamd_FOV :: Double
}
    deriving Show


data GatherApprox
data GatherRaytrace
data GatherNone

data GatherMethod gm where
    GatherApprox :: GatherMethod GatherApprox
    GatherRaytrace :: GatherMethod GatherRaytrace
    GatherNone :: GatherMethod GatherNone

deriving instance Show (GatherMethod a)

class IsNotGatherNone gm
instance IsNotGatherNone GatherApprox
instance IsNotGatherNone GatherRaytrace

defaultGatherApprox :: GatherMethod GatherApprox
defaultGatherApprox = GatherApprox
defaultGatherRaytrace :: GatherMethod GatherRaytrace
defaultGatherRaytrace = GatherRaytrace

data EnvLightSettings gm where
    NoEnvLight :: EnvLightSettings gm 
    EnvLight :: IsNotGatherNone gm => BlenderEnergy -> EnvLightSettings gm

deriving instance Show (EnvLightSettings gm)

data WorldLightSettings = forall gm. WorldLightSettings {
    wls_gatherMethod :: GatherMethod gm,
    wls_envLight :: EnvLightSettings gm
}

deriving instance Show WorldLightSettings

data BlenderWorld = BWorld {
    bworld_light :: WorldLightSettings,
    bworld_otherProps :: Props
}

nameMakeLens ''BlenderWorld (Just . (++"L"))

deriving instance Show BlenderWorld



defaultWorld :: BlenderWorld 
defaultWorld = 
    BWorld {
        bworld_light = WorldLightSettings {
            wls_gatherMethod = GatherNone,
            wls_envLight = NoEnvLight
        },
        bworld_otherProps =
            ["use_sky_blend" & False,
             "use_sky_real" & False,
             "horizon_color" & ((1,1,1)::(Int,Int,Int))
            ]
    }


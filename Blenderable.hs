{-# LANGUAGE FunctionalDependencies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, DeriveGeneric, ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, GADTs, TypeFamilies, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Blenderable where


import Data.Function
import Data.List
import Data.Vect.Double
import DisjointUnion
import HomogenousTuples
import Prelude hiding(catch,mapM_,sequence_) 
import Simplicial.DeltaSet
import ToPython
import PreRenderable
import GHC.Generics
import SimplicialPartialQuotient
import Simplicial.SimplicialComplex
import ConcreteNormal.PreRenderable
import NormalEverything
import Simplicial.AnySimplex
import Control.Exception
import PrettyUtil
import ShortShow

data Blenderable a = Blenderable { 
    ba_ds :: DeltaSet a,
    ba_coords :: Vert a -> Vec3,
    ba_faceInfo :: AnySimplex a -> FaceInfo,
    ba_vertexThickness :: Vert a -> Double,
    ba_visible :: AnySimplex a -> Bool,
    ba_edgeThickness :: Arc a -> Double,
    ba_triangleLabel :: Tri a -> Maybe TriangleLabel, 
    ba_materials :: [Material]
}
    deriving (Generic)



instance Coords (Blenderable a) where
    transformCoords f b = b { ba_coords = f . ba_coords b }

data FaceInfo = FaceInfo {
        faceName :: String,
        faceMat :: Material
}

-- instance (BlenderLabel a, BlenderLabel b) => BlenderLabel (DisjointUnion a b) where
--     blenderLabel (DisjointUnion a b) n = blenderLabel a n ||| blenderLabel b n 

type MatName = String


type EulerAnglesXYZ = Vec3

eulerAnglesXYZ :: Double -> Double -> Double -> EulerAnglesXYZ
eulerAnglesXYZ = Vec3

data Scene a = Scene {
    scene_blenderable :: Blenderable a,
    scene_worldProps :: Props,
    scene_cams :: [Cam]
}

deriving instance (Pretty (Vert a), ShowN a) => Show (Scene a)


data Cam = Cam {
    cam_pos :: Vec3,
    -- | XYZ eulers
    cam_eulers :: EulerAnglesXYZ,
    cam_FOV :: Double
}
    deriving(Show)

defaultFOV :: Double
defaultFOV = 0.8575560591178853

setCams :: [Cam] -> Scene a -> Scene a
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

-- newtype Pseudomanifold a = Pseudomanifold { unPseudomanifold :: a }
--     deriving(Coordinates)


pmMat0 :: Material
pmMat0 = Material "pmMat0" (let r=0.3 in diffuseColor (r, r, r):[])
pmMat1 :: Material
pmMat1 = Material "pmMat1" (diffuseColor (0.7, 0.7, 0.8):specular 100 0.8)
pmMat2 :: Material
pmMat2 = Material "pmMat2" (diffuseColor (0.7, 0.7, 0.8):specular 100 0.8++transparency 0.25 0.3 1)


pmVertThickness ::  Double
pmVertThickness = 0.04
edgeThicknessFactor ::  Double
edgeThicknessFactor = 0.4
nsurfVertThickness ::  Double
nsurfVertThickness = 0.03



pseudomanifoldStyle
  :: PreRenderable a -> Blenderable a
pseudomanifoldStyle = mkBlenderable pmMat0 pmMat1 pmMat2 pmVertThickness  

mkBlenderable
  :: forall a. 
     Material
     -> Material
     -> Material
     -> Double
     -> PreRenderable a
     -> Blenderable a
mkBlenderable mat0 mat1 mat2 vertThick pr_ = Blenderable { 
    ba_ds = pr_ds pr_,
    ba_coords = pr_coords pr_,
    ba_triangleLabel = pr_triangleLabel pr_,
    ba_visible = pr_visible pr_,

    ba_faceInfo = \asi -> elimAnySimplexWithNat asi (\n _ ->
        FaceInfo 
            (pr_name pr_ asi) 
            (caseNat3 n mat0 mat1 mat2 (assert False undefined))),

    ba_vertexThickness = const vertThick,
    ba_edgeThickness = const (edgeThicknessFactor*vertThick),

    ba_materials = [mat0,mat1,mat2]
}
--     where
--         sh :: forall n. Nat n => n -> a n -> String
--         sh _ = showN 



nsurfMat0 ::  Material
nsurfMat0 = Material "nsurfMat0" (diffuseColor (0, 0.4, 1):[])
nsurfMat1 ::  Material
nsurfMat1 = Material "nsurfMat1" (diffuseColor (0, 0.2, 1):specular 100 0.8)
nsurfMat2 ::  Material
nsurfMat2 = Material "nsurfMat2" (diffuseColor (0, 0, 1):specular 100 0.8++transparency 0.55 0.7 1)

normalSurfaceStyle :: PreRenderable a -> Blenderable a
normalSurfaceStyle = mkBlenderable nsurfMat0 nsurfMat1 nsurfMat2 nsurfVertThickness


-- | Points cam at positive y dir
defaultScene :: ToBlenderable x a => x -> Scene a
defaultScene a = Scene (toBlenderable a) defaultWorldProps [defaultCam]

defaultCam :: Cam
defaultCam = Cam (Vec3 0.66 (-2.3) 0.52) (eulerAnglesXYZ (pi/2) 0 0) defaultFOV

(&) :: ToPython a => t -> a -> (t, Python ())
x & y = (x, toPython y)

defaultWorldProps :: Props
defaultWorldProps = ["use_sky_blend" & False,
                     "use_sky_real" & False,
                     "horizon_color" & ((1,1,1)::(Int,Int,Int))
                     ]



instance DisjointUnionable [Material] [Material] [Material] where
    disjointUnion ma ma' =
            (nubBy ((==) `on` ma_name) (ma++ma'))

instance DisjointUnionable (Blenderable a) (Blenderable b) (Blenderable (Either1 a b)) where

    disjointUnion = defaultDisjointUnion






--normalSurface' a = normalSurface (addCoordFunc id (const Nothing) a)


instance (Pretty (Vert a), ShowN a) => Pretty (Blenderable a) where
    pretty Blenderable{..} = prettyRecord "Blenderable"
        [("ba_ds",pretty ba_ds),
         ("ba_coords",prettyFunction ba_coords (vertices ba_ds))
        ]


instance (Pretty (Vert a), ShowN a) => Show (Blenderable a) where
    showsPrec = prettyShowsPrec



-- * Conversion

class ToBlenderable x a | x -> a where
    toBlenderable :: x -> Blenderable a

instance ToBlenderable (Blenderable a) a where
    toBlenderable = id

fromSpqwc
  :: (Ord v, ShortShow v,Pretty v) =>
     SPQWithCoords v
     -> Blenderable (OTuple v)
fromSpqwc = pseudomanifoldStyle . toPreRenderable

-- | = 'fromSpqwc'
instance (Ord v, ShortShow v, Pretty v) => ToBlenderable (SPQWithCoords v) (OTuple v) where
    toBlenderable = fromSpqwc

fromNormalSurface
  :: (Integral i, Ord v, Pretty i, ShortShow v, NormalSurface s i) =>
     SPQWithCoords v -> s -> Blenderable (OTuple (Corn v))
fromNormalSurface spqwc stc = normalSurfaceStyle (normalSurfaceToPreRenderable spqwc stc) 

fromIntegerNormalSurface
  :: (Ord v, ShortShow v, NormalSurface s Integer) =>
     SPQWithCoords v -> s -> Blenderable (OTuple (Corn v))
fromIntegerNormalSurface = fromNormalSurface


fromSpqwcAndIntegerNormalSurface
  :: (Ord v, ShortShow v, NormalSurface s Integer, Pretty v) =>
     SPQWithCoords v
     -> s -> Blenderable (Either1 (OTuple v) (OTuple (Corn v)))
fromSpqwcAndIntegerNormalSurface spqwc s = 
    fromSpqwc spqwc `disjointUnion`
    fromIntegerNormalSurface spqwc s 


-- | = @uncurry 'fromSpqwcAndIntegerNormalSurface'@
instance (Ord v, ShortShow v, NormalSurface s Integer, Pretty v) => 
    ToBlenderable (SPQWithCoords v, s) (Either1 (OTuple v) (OTuple (Corn v))) where

    toBlenderable = uncurry fromSpqwcAndIntegerNormalSurface

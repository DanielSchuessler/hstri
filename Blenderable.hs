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

data Blenderable s = Blenderable { 
    ba_ds :: s,
    ba_coords :: Vert s -> Vec3,
    ba_faceInfo :: AnySimplex2Of s -> FaceInfo,
    ba_vertexThickness :: Vert s -> Double,
    ba_visible :: AnySimplex2Of s -> Bool,
    ba_edgeThickness :: Arc s -> Double,
    ba_triangleLabel :: Tri s -> Maybe TriangleLabel, 
    ba_materials :: [Material]
}
    deriving (Generic)



instance Coords (Blenderable s) where
    transformCoords f b = b { ba_coords = f . ba_coords b }

data FaceInfo = FaceInfo {
        faceName :: String,
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

-- newtype Pseudomanifold s = Pseudomanifold { unPseudomanifold :: s }
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
  :: PreRenderable s -> Blenderable s
pseudomanifoldStyle = mkBlenderable pmMat0 pmMat1 pmMat2 pmVertThickness  

mkBlenderable
  :: forall s. 
     Material
     -> Material
     -> Material
     -> Double
     -> PreRenderable s
     -> Blenderable s
mkBlenderable mat0 mat1 mat2 vertThick pr_ = Blenderable { 
    ba_ds = pr_ds pr_,
    ba_coords = pr_coords pr_,
    ba_triangleLabel = pr_triangleLabel pr_,
    ba_visible = pr_visible pr_,

    ba_faceInfo = \asi ->
        FaceInfo 
            (pr_name pr_ asi) 
            (foldAnySimplex2 (const mat0) (const mat1) (const mat2) asi),

    ba_vertexThickness = const vertThick,
    ba_edgeThickness = const (edgeThicknessFactor*vertThick),

    ba_materials = [mat0,mat1,mat2]
}
--     where
--         sh :: forall n. Nat n => n -> s n -> String
--         sh _ = showN 



nsurfMat0 ::  Material
nsurfMat0 = Material "nsurfMat0" (diffuseColor (0, 0.4, 1):[])
nsurfMat1 ::  Material
nsurfMat1 = Material "nsurfMat1" (diffuseColor (0, 0.2, 1):specular 100 0.8)
nsurfMat2 ::  Material
nsurfMat2 = Material "nsurfMat2" (diffuseColor (0, 0, 1):specular 100 0.8++transparency 0.55 0.7 1)

normalSurfaceStyle :: PreRenderable s -> Blenderable s
normalSurfaceStyle = mkBlenderable nsurfMat0 nsurfMat1 nsurfMat2 nsurfVertThickness


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



instance DisjointUnionable [Material] [Material] [Material] where
    disjointUnion ma ma' =
            (nubBy ((==) `on` ma_name) (ma++ma'))


instance 
    (   DisjointUnionable s1 s2 s
    
    ,   CoDisjointUnionable
                        (Vert s1) (Vert s2) (Vert s)
    ,   CoDisjointUnionable
                        (Ed s1) (Ed s2) (Ed s)
    ,   CoDisjointUnionable
                        (Tri s1) (Tri s2) (Tri s)
    ) =>
    DisjointUnionable (Blenderable s1) (Blenderable s2) (Blenderable s) where



    disjointUnion = defaultDisjointUnion






--normalSurface' s = normalSurface (addCoordFunc id (const Nothing) s)


instance (Pretty (Vert s), Vertices s, Pretty s) => Pretty (Blenderable s) where
    pretty Blenderable{..} = prettyRecord "Blenderable"
        [("ba_ds",pretty ba_ds),
         ("ba_coords",prettyFunction ba_coords (vertices ba_ds))
        ]


instance (Pretty (Vert s), Vertices s, Pretty s) => Show (Blenderable s) where
    showsPrec = prettyShowsPrec

deriving instance (Pretty (Vert s), Vertices s, Pretty s) => Show (Scene s)

-- * Conversion

class ToBlenderable x s | x -> s where
    toBlenderable :: x -> Blenderable s

instance ToBlenderable (Blenderable s) s where
    toBlenderable = id

fromSpqwc
  :: (Ord v, ShortShow v,Pretty v,Show v) =>
     SPQWithCoords v
     -> Blenderable (SC2 v)
fromSpqwc = pseudomanifoldStyle . pr_popDimension . toPreRenderable

-- | = 'fromSpqwc'
instance (Ord v, ShortShow v, Pretty v, Show v) => ToBlenderable (SPQWithCoords v) (SC2 v) where
    toBlenderable = fromSpqwc

fromNormalSurface
  :: (Integral i, Ord v, Pretty i, ShortShow v, NormalSurface s i, Show v) =>
     SPQWithCoords v -> s -> Blenderable (SC2 (Corn v))
fromNormalSurface spqwc stc = normalSurfaceStyle (normalSurfaceToPreRenderable spqwc stc) 

fromIntegerNormalSurface
  :: (Ord v, ShortShow v, NormalSurface s Integer, Show v) =>
     SPQWithCoords v -> s -> Blenderable (SC2 (Corn v))
fromIntegerNormalSurface = fromNormalSurface


fromSpqwcAndIntegerNormalSurface
  :: (Ord v, ShortShow v, NormalSurface s Integer, Pretty v, Show v) =>
     SPQWithCoords v
     -> s -> Blenderable (DJ (SC2 v) (SC2 (Corn v)))
fromSpqwcAndIntegerNormalSurface spqwc s = 
    fromSpqwc spqwc 
    `disjointUnion`
    fromIntegerNormalSurface spqwc s 


-- | = @uncurry 'fromSpqwcAndIntegerNormalSurface'@
instance (Ord v, ShortShow v, NormalSurface s Integer, Pretty v, Show v) => 
    ToBlenderable (SPQWithCoords v, s) (DJ (SC2 v) (SC2 (Corn v))) where

    toBlenderable = uncurry fromSpqwcAndIntegerNormalSurface

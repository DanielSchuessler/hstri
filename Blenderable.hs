{-# LANGUAGE ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, GADTs, TypeFamilies, NamedFieldPuns #-}
{-# OPTIONS -Wall #-}
module Blenderable where


import Data.Function
import Data.List
import Data.Vect.Double
import DisjointUnion
import HomogenousTuples
import Prelude hiding(catch,mapM_,sequence_) 
import Simplicial.DeltaSet
import Simplicial.Labels
import ToPython
import Simplicial.AnySimplex

data Blenderable a = Blenderable { 
    ba_main :: LabeledDeltaSet a BlenderSimplexInfo,
    ba_materials :: [Material]
}
    deriving Show

data BlenderSimplexInfo n where
    BSI0 :: { 
        bsiCoords :: Vec3,
        faceInfo0 :: FaceInfo,
        vertexThickness :: Double
     } -> BlenderSimplexInfo N0
    BSI1 :: { 
        faceInfo1 :: FaceInfo,
        edgeThickness :: Double
     } -> BlenderSimplexInfo N1
    BSI2 :: { 
        faceInfo2 :: FaceInfo,
        triangleLabel :: Maybe TriangleLabel
     } -> BlenderSimplexInfo N2
    BSIOther :: BlenderSimplexInfo (S (S (S n)))


instance Coords (BlenderSimplexInfo n) where
    transformCoords f b@BSI0{bsiCoords} = b { bsiCoords = f bsiCoords } 
    transformCoords _ x = x

instance Coords (Blenderable a) where
    transformCoords f b = b { ba_main = mapSimplexLabels (transformCoords f) (ba_main b) } 

data FaceInfo = FaceInfo {
        faceLabel :: String,
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
    deriving Show


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
                "raytrace_transparency.depth" & (5::Int)
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



pseudomanifold
  :: (Show (Vert a), Show (Arc a), Show (Tri a)) =>
     WithCoords a -> Blenderable a
pseudomanifold = mkBlenderable pmMat0 pmMat1 pmMat2 pmVertThickness  

mkBlenderable
  :: forall a. (Show (Vert a), Show (Arc a), Show (Tri a)) =>
     Material
     -> Material
     -> Material
     -> Double
     -> WithCoords a
     -> Blenderable a
mkBlenderable mat0 mat1 mat2 vertThick a = Blenderable { 
    ba_main = mapSimplexLabelsWithSimplices 
        (\s l -> 
            case l of
                CoordLabelsF0 x ->
                    (BSI0 x (FaceInfo (show s) mat0) vertThick)
                CoordLabelsF1 ->
                    (BSI1 (FaceInfo (show s) mat1) (edgeThicknessFactor*vertThick))
                CoordLabelsF2 x ->
                    (BSI2 (FaceInfo (show s) mat2) x)
                CoordLabelsF3 ->
                     BSIOther)
    
        a
        ,
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

normalSurface
  :: (Show (Vert a), Show (Arc a), Show (Tri a)) =>
     WithCoords a -> Blenderable a
normalSurface = mkBlenderable nsurfMat0 nsurfMat1 nsurfMat2 nsurfVertThickness


-- | Points cam at positive y dir
defaultScene ::  Blenderable a -> Scene a
defaultScene a = Scene a defaultWorldProps [defaultCam]

defaultCam :: Cam
defaultCam = Cam (Vec3 0.66 (-2.3) 0.52) (eulerAnglesXYZ (pi/2) 0 0) defaultFOV

(&) ::  ToPython a => t -> a -> (t, Python ())
x & y = (x, toPython y)

defaultWorldProps :: Props
defaultWorldProps = ["use_sky_blend" & False,
                     "use_sky_real" & False,
                     "horizon_color" & ((1,1,1)::(Int,Int,Int))
                     ]


instance DisjointUnionable (Blenderable a) (Blenderable b) where
    type DisjointUnion (Blenderable a) (Blenderable b) = Blenderable (Either1 a b)

    disjointUnion (Blenderable a ma) (Blenderable a' ma') =
        Blenderable 
            (disjointUnion a a') 
            (nubBy ((==) `on` ma_name) (ma++ma'))


ba_ds :: Blenderable a -> DeltaSet a
ba_ds = lds_ds . ba_main

ba_simplbl :: Nat n => Blenderable a -> (a n) -> BlenderSimplexInfo n
ba_simplbl a = simplbl (ba_main a)

ba_coords :: Blenderable a -> (a   N0) -> Vec3
ba_coords a = bsiCoords . ba_simplbl a

--normalSurface' a = normalSurface (addCoordFunc id (const Nothing) a)



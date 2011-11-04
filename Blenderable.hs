{-# LANGUAGE ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, GADTs, TypeFamilies, NamedFieldPuns #-}
{-# OPTIONS -Wall #-}
module Blenderable where


import Data.Function
import Data.List
import Data.Vect.Double
import DeltaSet
import DisjointUnion
import HomogenousTuples
import Prelude hiding(catch,mapM_,sequence_) 
import SimplexLabels
import ToPython
import TypeLevel.TF

data Blenderable a = Blenderable { 
    ba_main :: LabeledDeltaSet a (ApplyConstr BlenderSimplexInfo),
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
        faceInfo2 :: FaceInfo
     } -> BlenderSimplexInfo N2
    BSIOther :: BlenderSimplexInfo (S (S (S n)))

instance Coords (BlenderSimplexInfo n) where
    transformCoords f b@BSI0{bsiCoords} = b { bsiCoords = f bsiCoords } 
    transformCoords _ x = x

instance Coords (Blenderable a) where
    transformCoords f b = b { ba_main = mapSimplexLabels (\_ -> transformCoords f) (ba_main b) } 

data FaceInfo = FaceInfo {
        faceLabel :: String,
        faceMat :: Material
}

-- instance (BlenderLabel a, BlenderLabel b) => BlenderLabel (DisjointUnion a b) where
--     blenderLabel (DisjointUnion a b) n = blenderLabel a n ||| blenderLabel b n 

type MatName = String



data Scene a = Scene {
    scene_blenderable :: Blenderable a,
    scene_worldProps :: Props
}
    deriving Show
                        
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
  :: (ShowN a, (l :$ Z) ~ Vec3) =>
     LabeledDeltaSet a l -> Blenderable a
pseudomanifold = mkBlenderable pmMat0 pmMat1 pmMat2 pmVertThickness  

mkBlenderable
  :: forall a l. (ShowN a, (l :$ Z) ~ Vec3) =>
     Material
     -> Material
     -> Material
     -> Double
     -> LabeledDeltaSet a l
     -> Blenderable a
mkBlenderable mat0 mat1 mat2 vertThick a = Blenderable { 
    ba_main = mapSimplexLabelsWithSimplices 
        (\n s x -> caseNat3 n
                    (BSI0 x (FaceInfo (sh n0 s) mat0) vertThick)
                    (BSI1 (FaceInfo (sh n1 s) mat1) (edgeThicknessFactor*vertThick))
                    (BSI2 (FaceInfo (sh n2 s) mat2))
                    (const BSIOther))
    
        a
        ,
    ba_materials = [pmMat0,pmMat1,pmMat2]
}
    where
        sh :: forall n. Nat n => n -> a :$ n -> String
        sh = showN (undefined :: a)



nsurfMat0 ::  Material
nsurfMat0 = Material "nsurfMat0" (diffuseColor (0, 0.4, 1):[])
nsurfMat1 ::  Material
nsurfMat1 = Material "nsurfMat1" (diffuseColor (0, 0.2, 1):specular 100 0.8)
nsurfMat2 ::  Material
nsurfMat2 = Material "nsurfMat2" (diffuseColor (0, 0, 1):specular 100 0.8++transparency 0.55 0.7 1)

normalSurface
  :: (ShowN a, (l :$ Z) ~ Vec3) =>
     LabeledDeltaSet a l -> Blenderable a
normalSurface = mkBlenderable nsurfMat0 nsurfMat1 nsurfMat2 nsurfVertThickness



defaultScene ::  Blenderable a -> Scene a
defaultScene a = Scene a defaultWorldProps

(&) ::  ToPython a => t -> a -> (t, Python ())
x & y = (x, toPython y)

defaultWorldProps :: Props
defaultWorldProps = ["use_sky_blend" & False,
                     "use_sky_real" & False,
                     "horizon_color" & ((1,1,1)::(Int,Int,Int))
                     ]


instance DisjointUnionable (Blenderable a) (Blenderable b) where
    type DisjointUnion (Blenderable a) (Blenderable b) = Blenderable (EitherSequence a b)

    disjointUnion (Blenderable a ma) (Blenderable a' ma') =
        Blenderable 
            (disjointUnion a a') 
            (nubBy ((==) `on` ma_name) (ma++ma'))


ba_ds :: Blenderable a -> DeltaSet a
ba_ds = lds_ds . ba_main

ba_simplbl :: Nat n => Blenderable a -> n -> (a :$ n) -> BlenderSimplexInfo n
ba_simplbl a n = simplbl (ba_main a) n

ba_coords :: Blenderable a -> (a :$ N0) -> Vec3
ba_coords a = bsiCoords . ba_simplbl a n0

normalSurface'
  :: (ShowN a, (a :$ N0) ~ Vec3) => DeltaSet a -> Blenderable a
normalSurface' a = normalSurface (addCoordFunc id a)



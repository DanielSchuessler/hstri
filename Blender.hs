{-# LANGUAGE FlexibleContexts, TemplateHaskell, ScopedTypeVariables, PolymorphicComponents, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, CPP, GeneralizedNewtypeDeriving, TypeFamilies, DefaultSignatures, ExtendedDefaultRules, StandaloneDeriving #-} 
module Blender where

import Control.Arrow
import Control.Monad.Writer hiding(sequence_,mapM_)
import Data.Foldable
import Data.List
import Data.NaturalNumber
import Data.NaturalNumber
import Data.Vect.Double
import Data.Vect.Double.GramSchmidt
import Debug.Trace
import Prelude hiding(catch,mapM_,sequence_) 
import System.Environment
import System.Process
import Text.Printf.TH
import DeltaSet
import TypeLevel.NaturalNumber hiding(NaturalNumber)
import Control.Arrow
import SimplicialComplex
import MathUtil
import HomogenousTuples
import Data.Function
import TupleTH
import ToPython

#include "macros.h"

default(Int)

class Coordinates a => Blenderable a where
    faceInfo0 :: a -> Vert a -> FaceInfo 
    faceInfo1 :: a -> Delta1 a -> FaceInfo 
    faceInfo2 :: a -> Delta2 a -> FaceInfo 

    materials :: a -> [Material]



-- instance (BlenderLabel a, BlenderLabel b) => BlenderLabel (DisjointUnion a b) where
--     blenderLabel (DisjointUnion a b) n = blenderLabel a n ||| blenderLabel b n 

type MatName = String

data FaceInfo = FaceInfo {
    faceLabel :: String,
    faceMat :: Material,
    faceThickness :: Double
}


data Scene a = Scene {
    sceneTriang :: a,
    worldProps :: Props
}

defaultScene a = Scene a defaultWorldProps

x & y = (x, toPython y)

defaultWorldProps = ["use_sky_blend" & False,
                     "use_sky_real" & False,
                     "horizon_color" & (1,1,1)
                     ]

instance (Blenderable a, Blenderable b) => Blenderable (DisjointUnion a b) where
    materials = nubBy ((==) `on` ma_name) . foldMapDJ (++) materials materials
#define MK_FI(X) X = deriveDJ X X
    MK_FI(faceInfo0)
    MK_FI(faceInfo1)
    MK_FI(faceInfo2)
#undef MK_FI

instance Blenderable a => Blenderable (WithVertexCoordFunc a) where
    materials = materials . cp_complex
#define MK_FI(X) X = X . cp_complex
    MK_FI(faceInfo0)
    MK_FI(faceInfo1)
    MK_FI(faceInfo2)
#undef MK_FI

sceneVar = py "scene"
worldVar = py "world"
camVar = py "cam"
objVar = py "obj"
lightVar = py "light"



objSetName x = objVar <.> "name" .= str x

objSetMaterial x = objVar <.> "active_material" .= x 

ops x = py "ops" <.> x
dat x = py "data" <.> x
context x = py "context" <.> x

ma_var = py . ma_name

-- toBlender :: forall a. (BlenderLabel a, 
--                         VertexInfoClass (Vertex a),
--                         Show (Vertex a), 
--                         Show (Edge a), 
--                         Show (Triangle a), 
--                         DeltaSet a) => 
--        Scene a
--     -> Python ()
toBlender Scene{..} = result
    where
        result = do
            ln "from bpy import *"
            ln "from pprint import pprint"

            -- Make a new scene
            sceneVar .= methodCallExpr1 (dat "scenes") "new" (str "TheScene")
            -- Remove all other scenes
            ln $ $(printf "for s in data.scenes:\n  if (s != %s):\n    data.scenes.remove(s)") (renderPython sceneVar)

            -- Make a new world, assign it to the scene, set world properties
            worldVar .= methodCallExpr1 (dat "worlds") "new" (str "TheWorld")
            mapM_ (setProp' worldVar) worldProps
            setProp sceneVar "world" worldVar

            camVar .= methodCallExpr1 (dat "cameras") "new" (str "TheCam")
            newObj "TheCam" camVar
            objVar <.> "location" .= Vec3 0 (-5) 0
            objVar <.> "rotation_euler" .= Vec3 (pi/2) 0 0 -- point at positive y dir
            setProp sceneVar "camera" objVar

            lightVar .= methodCallExpr (dat "lamps") "new" (str "TheLamp", str "SUN") 
            lightVar <.> "shadow_method" .= str "RAY_SHADOW"

            newObj "TheLamp" lightVar
            objVar <.> "rotation_euler" .= Vec3 (5*pi/12) 0 (-pi/6)
            objVar <.> "location" .= Vec3 (-5) (-10) 8
            




            materialDefs
            mapM_ handleV (s0 sceneTriang)
            mapM_ handleE (s1 sceneTriang)
            mapM_ handleT (s2 sceneTriang)

        


        materialDefs = mapM_ materialToBlender (materials sceneTriang) 



        dims = [0..2]


        coords' = coords sceneTriang



        handleV v = do
                sphere (coords' v) faceThickness
                objSetName faceLabel
                objSetMaterial (ma_var faceMat)
            
            where
                FaceInfo {..} = faceInfo0 sceneTriang v 

        handleE e = do
                cylinder (coords' v0) (coords' v1) faceThickness
                objSetName faceLabel
                objSetMaterial (ma_var faceMat)
            

            where
                (v1,v0) = faces10 sceneTriang e
                FaceInfo {..} = faceInfo1 sceneTriang e


        handleT t = do
                    triangle (coords' v0) (coords' v1) (coords' v2)
                    objSetName faceLabel
                    objSetMaterial (ma_var faceMat)
                    objVar <.> "show_transparent" .= True

            where
                (v2,v1,v0) = faces20 sceneTriang t
                FaceInfo {..} = faceInfo2 sceneTriang t
        


        
        sphere loc radius = do
            methodCall1 (ops "surface") "primitive_nurbs_surface_sphere_add" 
                (namedArg "location" loc)

            assignObjFromContext

            objVar <.> "scale" .= (Vec3 radius radius radius)


        cylinder :: Vec3 -> Vec3 -> Double -> Python ()
        cylinder from to radius = do
            methodCall (ops "surface") "primitive_nurbs_surface_cylinder_add" ()

            assignObjFromContext

            objVar <.> "matrix_basis" .= m

            where
                m :: Proj4
                m = 
                    
                    scaling (Vec3 radius radius (1/2)) .*. 
                    translation ((1/2) *& vec3Z) .*.
                    linear (Mat3 x' y' z') .*.
                    translation from

                x',y',z' :: Vec3

                x' = normalize (anyOrth z')
                y' = normalize (crossprod x' z')
                z' = to &- from

        triangle p0 p1 p2 = mesh [p0,p1,p2] [(0,1,2)]








        
        assignObjFromContext = objVar .= (context "object")



testBlender s = do
    let fn = "/tmp/foo.py"
    writeFile fn (renderPython $ toBlender s)
    putStrLn fn
    args <- getArgs
    rawSystem "blender" ("-P":fn:args)


meshVar = py "me"

emptyList = [] :: [()]

mesh :: [Vec3] -> [(Int,Int,Int)] -> Python ()
mesh verts faces = 
            do 
            meshVar .= (methodCallExpr1 (dat "meshes") "new" (str "SomeMesh"))

            methodCall meshVar "from_pydata" (verts,emptyList,faces)


--             "print('vertices:')",
--             "for x in me.vertices:",
--             "   pprint(x.co)",

            methodCall meshVar "update" ()
            newObj "SomeMesh" meshVar 

-- | Returns the object in the 'objVar' \"register\" :-)
newObj name objData = do
            objVar .= (methodCallExpr (dat "objects") "new" (str name,objData))
            methodCall1 (sceneVar <.> "objects") "link" objVar



--linkObjToScene =


data Material = Material {
    ma_name :: MatName,
    ma_props :: Props
}

materialToBlender :: Material -> Python ()
materialToBlender m@Material{..} = do

    let var = ma_var m

    var .= methodCallExpr1 (dat "materials") "new" (str ma_name)

    let ma_props' = ("use_transparent_shadows" & True)
                        : ma_props
    mapM_ (setProp' var) ma_props'


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

transparency alpha spec_alpha fresnel =
                [
                "use_transparency" & True,
                "transparency_method" & str "RAYTRACE",
                -- "transparency_method" & "'Z_TRANSPARENCY'",
                "alpha" & (alpha::Double),
                "specular_alpha" & (spec_alpha::Double),
                "translucency" & (1-alpha),
                "raytrace_transparency.fresnel_factor" & 1,
                "raytrace_transparency.fresnel" & (fresnel::Double),
                "raytrace_transparency.depth" & 5
                ]

newtype Pseudomanifold a = Pseudomanifold { unPseudomanifold :: a }
    deriving(Coordinates)

DERIVE_DELTA_SET(Pseudomanifold a, unPseudomanifold)

pmMat0 = Material "pmMat0" (let r=0.3 in diffuseColor (r, r, r):[])
pmMat1 = Material "pmMat1" (diffuseColor (0.7, 0.7, 0.8):specular 100 0.8)
pmMat2 = Material "pmMat2" (diffuseColor (0.7, 0.7, 0.8):specular 100 0.8++transparency 0.25 0.3 1)


pmVertThickness = 0.04
edgeThicknessFactor = 0.4
nsurfVertThickness = 0.03

instance Coordinates a => Blenderable (Pseudomanifold a) where
    faceInfo0 _ _ = (FaceInfo "pm0" pmMat0 pmVertThickness)
    faceInfo1 _ _ = (FaceInfo "pm1" pmMat1 (edgeThicknessFactor*pmVertThickness))
    faceInfo2 _ _ = (FaceInfo "pm2" pmMat2 0)
    materials _ = [pmMat0,pmMat1,pmMat2]

newtype NormalSurface a = NormalSurface { unNormalSurface :: a }
    deriving(Coordinates)

DERIVE_DELTA_SET(NormalSurface a, unNormalSurface)

nsurfMat0 = Material "nsurfMat0" (diffuseColor (0, 0.4, 1):[])
nsurfMat1 = Material "nsurfMat1" (diffuseColor (0, 0.2, 1):specular 100 0.8)
nsurfMat2 = Material "nsurfMat2" (diffuseColor (0, 0, 1):specular 100 0.8++transparency 0.55 0.7 1)


instance Coordinates a => Blenderable (NormalSurface a) where
    faceInfo0 _ _ = (FaceInfo "nsurf0" nsurfMat0 nsurfVertThickness)
    faceInfo1 _ _ = (FaceInfo "nsurf1" nsurfMat1 (edgeThicknessFactor * nsurfVertThickness))
    faceInfo2 _ _ = (FaceInfo "nsurf2" nsurfMat2 0)
    materials _ = [nsurfMat0,nsurfMat1,nsurfMat2]



-- materialDefs :: Python ()
-- materialDefs = do
--         sequence_ [ newMaterial name name | k <- [minBound .. maxBound], i <- dims, 
--                                     let name = faceMat k i ]
-- 
-- 
--         matSetTransp facMatVar 0.1 0.3 
--         matSetTransp (faceMat NormalSurfMat 2) 0.2 0.35
-- 
--         sequence_ [ matSetDiffuseColor (faceMat NormalSurfMat i) 
--                     (0,0.2,1)
--         
--                     | i <- dims ]


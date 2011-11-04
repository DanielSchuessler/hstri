{-# LANGUAGE GADTs, NamedFieldPuns, FlexibleContexts, TemplateHaskell, ScopedTypeVariables, PolymorphicComponents, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, CPP, GeneralizedNewtypeDeriving, TypeFamilies, DefaultSignatures, ExtendedDefaultRules, StandaloneDeriving #-} 
-- {-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Blender where

import Data.Foldable
import Data.Function
import Data.Vect.Double
import DeltaSet
import MathUtil
import Prelude hiding(catch,mapM_,sequence_) 
import System.Environment
import System.Process
import Text.Printf.TH
import ToPython
import Blenderable
import TypeLevel.TF






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
bpy_props = py "props"
types x = py "types" <.> x

ma_var = py . ma_name


-- toBlender :: forall a. (BlenderLabel a, 
--                         VertexInfoClass (Vertex a),
--                         Show (Vertex a), 
--                         Show (Edge a), 
--                         Show (Triangle a), 
--                         DeltaSet a) => 
--        Scene a
--     -> Python ()
toBlender :: Scene a -> Python ()
toBlender Scene{scene_worldProps,scene_blenderable=blenderable} = result
    where
        deltaSet = ba_ds blenderable        

        result = do
            ln "from bpy import *"
            ln "from pprint import pprint"

            -- Make a new scene
            sceneVar .= methodCallExpr1 (dat "scenes") "new" (str "TheScene")
            -- Remove all other scenes
            ln $ $(printf "for s in data.scenes:\n  if (s != %s):\n    data.scenes.remove(s)") (renderPython sceneVar)

            -- Make a new world, assign it to the scene, set world properties
            worldVar .= methodCallExpr1 (dat "worlds") "new" (str "TheWorld")
            mapM_ (setProp' worldVar) scene_worldProps
            setProp sceneVar "world" worldVar

            -- camera
            camVar .= methodCallExpr1 (dat "cameras") "new" (str "TheCam")
            newObj "TheCam" camVar
            objVar <.> "location" .= Vec3 0 (-5) 0
            objVar <.> "rotation_euler" .= Vec3 (pi/2) 0 0 -- point at positive y dir
            setProp sceneVar "camera" objVar

            -- light
            lightVar .= methodCallExpr (dat "lamps") "new" (str "TheLamp", str "SUN") 
            lightVar <.> "shadow_method" .= str "RAY_SHADOW"
            newObj "TheLamp" lightVar
            objVar <.> "rotation_euler" .= Vec3 (5*pi/12) 0 (-pi/6)
            objVar <.> "location" .= Vec3 (-5) (-10) 8

            materialDefs

            -- create object groups
            grp0 <- newGroup "grp0" "Vertices" 
            grp1 <- newGroup "grp1" "Edges" 
            grp2 <- newGroup "grp2" "Triangles" 

            (types "Object" <.> "simplexlabel") .= 
                methodCallExpr1 bpy_props "StringProperty" (str "Simplex label")


            mapM_ (handleV grp0) (s0 deltaSet)
            mapM_ (handleE grp1) (s1 deltaSet)
            mapM_ (handleT grp2) (s2 deltaSet)

        


        materialDefs = mapM_ materialToBlender (ba_materials blenderable) 





        coords' = ba_coords blenderable


        objCommon grp faceLabel faceMat = do
                objSetName faceLabel
                objSetMaterial (ma_var faceMat)
                grpLink grp objVar
                objVar <.> "simplexlabel" .= str faceLabel


        handleV grp v = do
                sphere (coords' v) vertexThickness
                objCommon grp faceLabel faceMat
            
            where
                BSI0 {..} = ba_simplbl blenderable n0 v 
                FaceInfo {..} = faceInfo0

        handleE grp e = do
                cylinder (coords' v0) (coords' v1) edgeThickness
                objCommon grp faceLabel faceMat
            

            where
                (v1,v0) = faces10 deltaSet e
                BSI1 {..} = ba_simplbl blenderable n1 e
                FaceInfo {..} = faceInfo1


        handleT grp t = do
                    triangle (coords' v0) (coords' v1) (coords' v2)
                    objVar <.> "show_transparent" .= True
                    objCommon grp faceLabel faceMat

            where
                (v2,v1,v0) = faces20 deltaSet t
                BSI2 {..} = ba_simplbl blenderable n2 t
                FaceInfo {..} = faceInfo2

        


        
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


newGroup :: String -> String -> Python (Python ())
newGroup varName groupName = do
    let var = py varName
    var .= methodCallExpr1 (dat "groups") "new" (str groupName) <.> "objects" 
    return var


testBlender s = do
    let fn = "/tmp/foo.py"
    writeFile fn (renderPython $ toBlender s)
    putStrLn fn
    args <- getArgs
    rawSystem "blender" ("-P":fn:args)


meshVar = py "me"

emptyList = [] :: [()]

mesh :: [Vec3] -> [(Int,Int,Int)] -> Python ()
mesh verts faceList = 
            do 
            meshVar .= (methodCallExpr1 (dat "meshes") "new" (str "M"))

            methodCall meshVar "from_pydata" (verts,emptyList,faceList)


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

grpLink :: Python () -> Python () -> Python ()
grpLink gr obj = methodCall1 gr "link" obj


materialToBlender :: Material -> Python ()
materialToBlender m@Material{..} = do

    let var = ma_var m

    var .= methodCallExpr1 (dat "materials") "new" (str ma_name)

    let ma_props' = ("use_transparent_shadows" & True)
                        : ma_props
    mapM_ (setProp' var) ma_props'





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


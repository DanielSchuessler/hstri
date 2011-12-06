{-# LANGUAGE GADTs, NamedFieldPuns, FlexibleContexts, TemplateHaskell, ScopedTypeVariables, PolymorphicComponents, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, CPP, GeneralizedNewtypeDeriving, TypeFamilies, DefaultSignatures, ExtendedDefaultRules, StandaloneDeriving #-} 
-- {-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Blender(
    module Blenderable,
    module MathUtil,
    testBlender,
    toBlender) where

import Blenderable
import Control.Exception
import Control.Monad(when)
import Data.Foldable
import Data.Function
import Data.Vect.Double hiding((.*),(.*.))
import HomogenousTuples
import MathUtil
import PreRenderable
import Prelude hiding(catch,mapM_,sequence_) 
import Simplicial.DeltaSet
import System.Environment
import System.Process
import Text.Printf.TH
import ToPython
import TypeLevel.TF
import qualified Util
import FaceClasses





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

triLabelMat = Material "triLbl" [ let r = 0.015 in diffuseColor (r,r,r) ]

-- toBlender :: forall a. (BlenderLabel a, 
--                         VertexInfoClass (Vertex a),
--                         Show (Vertex a), 
--                         Show (Edge a), 
--                         Show (blenderTriangle a), 
--                         DeltaSet a) => 
--        Scene a
--     -> Python ()
toBlender :: Scene a -> Python ()
toBlender scene@Scene{
        scene_worldProps,
        scene_blenderable=Blenderable{..},
        scene_cams} = result
    where
        deltaSet = ba_ds

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

            -- cameras
            forM_ (zip [0..] scene_cams) (\(i, (Cam pos eulers fov)) -> do
                let nam = "cam"++show i
                camVar .= methodCallExpr1 (dat "cameras") "new" (str nam)
                camVar <.> "angle" .= fov
                newObj nam camVar
                objVar <.> "location" .= pos
                objVar <.> "rotation_euler" .= eulers
                when (i==0) (setProp sceneVar "camera" objVar))

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
            grpTriLabels <- newGroup "grpTriLabels" "Triangle labels" 

            (types "Object" <.> "simplexlabel") .= 
                methodCallExpr1 bpy_props "StringProperty" (str "Simplex label")


            mapM_ (handleV grp0) (vertices deltaSet)
            mapM_ (handleE grp1) (edges deltaSet)
            mapM_ (handleT grp2 grpTriLabels) (triangles deltaSet)

        


        materialDefs = mapM_ materialToBlender (triLabelMat : ba_materials ) 





        coords' = ba_coords 


        objCommon grp faceName faceMat = do
                objSetName faceName
                objSetMaterial (ma_var faceMat)
                grpLink grp objVar
                objVar <.> "simplexlabel" .= str faceName


        handleV grp v = do
                sphere (coords' v) (ba_vertexThickness v)
                objCommon grp faceName faceMat
            
            where
                FaceInfo{..} = ba_faceInfo0 v

        handleE grp e = when (ba_visible1 e) $ do
                cylinder (coords' v0) (coords' v1) (ba_edgeThickness e)
                objCommon grp faceName faceMat
            

            where
                (v1,v0) = faces10 deltaSet e
                FaceInfo{..} = ba_faceInfo1 e


        handleT grp grpTriLabels t = do
                    blenderTriangle cv0 cv1 cv2 
                    objVar <.> "show_transparent" .= True
                    objCommon grp faceName faceMat

                    case ba_triangleLabel t of
                        Just (TriangleLabel lblstr g upDisplacementFactor) -> 
                            let (cu0,cu1,cu2) = g .* cvs 
                                right_ = normalize (cu1 &- cu0)
                                basepoint = interpolate 0.5 cu0 cu1
                                up1 = let up0 = (cu2 &- basepoint) 
                                      in up0 &- (dotprod right_ up0) *& right_
                                height = norm up1
                                up_ = up1 &* recip height 
                                m = 
                                    scalingUniformProj4 (0.4 * height)
                                    .*.
                                    safeOrthogonal (Mat3 right_ up_ (crossprod right_ up_)) 
                                    .*.
                                    translation (basepoint &+ upDisplacementFactor *& up1)



                            in do
                             newTextObj lblstr
                             objVar <.> matrix_basis .= m
                             objCommon grpTriLabels (lblstr ++ " on "++show g ++" "++faceName) triLabelMat 

                        Nothing -> return ()

            where
                vs = faces20Ascending deltaSet t
                cvs@(cv0,cv1,cv2) = map3 coords' vs

                FaceInfo{..} = ba_faceInfo2 t

        


        
        sphere loc radius = do
            methodCall1 (ops "surface") "primitive_nurbs_surface_sphere_add" 
                (namedArg "location" loc)

            assignObjFromContext

            objVar <.> "scale" .= (Vec3 radius radius radius)


        cylinder :: Vec3 -> Vec3 -> Double -> Python ()
        cylinder from to radius = do
            methodCall (ops "surface") "primitive_nurbs_surface_cylinder_add" ()

            assignObjFromContext

            objVar <.> matrix_basis .= m

            where
                m :: Proj4
                m = 
                    
                    scaling (Vec3 radius radius (1/2)) .*. 
                    translation ((1/2) *& vec3Z) .*.
                    linear (pointZTo (to &- from)) .*.
                    translation from






        
        assignObjFromContext = objVar .= (context "object")

matrix_basis = "matrix_basis"

blenderTriangle p0 p1 p2 =
    mesh [p0,p1,p2] [(0,1,2)]


safeOrthogonal :: Mat3 -> Proj4
safeOrthogonal m = 
    assert (matrixApproxEq (m .*. transpose m) idmtx) $
    assert (matrixApproxEq (transpose m .*. m) idmtx) $
    linear m


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

txtVar = py "txt"

textThickness :: Double
textThickness = 1E-5

newTextObj txt = do
    txtVar .= methodCallExpr (dat "curves") "new" (str "T", str "FONT")
    txtVar <.> "body" .= str txt
    txtVar <.> "extrude" .= textThickness
    txtVar <.> "align" .= str "CENTER"
    newObj "T" txtVar


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


{-# LANGUAGE ImplicitParams, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module Blender.Build where

import Blender.Types
import ToPython
import Data.Vect.Double.Base
import Blender.Blenderable
import Data.Vect.Double.Util.Dim3
import MathUtil
import Control.Exception
import Control.Monad.IfElse
import Blender.Mesh
import Blender.ID
import Control.Monad
import Data.Vect.Double.Util.Projective(translateAfter4)


contextObject :: Python ()
contextObject = py "context.object"


newSphereObj :: ToPython r => PythonVar -> r -> Double -> Python ()
newSphereObj (var::PythonVar) loc radius = do
            methodCall1 (bpy_ops "surface") "primitive_nurbs_surface_sphere_add" 
                    (namedArg "location" loc)
            var .= contextObject
            var <.> "scale" .= (Vec3 radius radius radius)




newCylinderObj
  :: (?sceneE::Python ()) =>
     PythonVar -> Vec3 -> Vec3 -> BlenderUnits -> Python ()
newCylinderObj (var::PythonVar) from to radius = 
    if normsqr (from &- to) <= 1E-14
       then newEmptyObj var "Crushed newCylinderObj"
       else 

        do
            methodCall (bpy_ops "surface") "primitive_nurbs_surface_cylinder_add" ()
            var .= contextObject
            var <.> matrix_basis .= cylinderTransform from to radius



newCylinderMeshObj
  :: PythonVar -> Vec3 -> Vec3 -> BlenderUnits -> Int -> Bool -> Python ()
newCylinderMeshObj (var::PythonVar) from to radius _nvertices smooth = do
    methodCall1 (bpy_ops "mesh") "primitive_cylinder_add" (namedArg "vertices" _nvertices)
    var .= contextObject
    var <.> matrix_basis .= cylinderTransform from to radius
    when smooth $ mesh_setSmooth (var <.> "data")




cylinderTransform :: Vec3 -> Vec3 -> BlenderUnits -> Proj4
cylinderTransform from to radius =
                    scaling (Vec3 radius radius (1/2)) .*. 
                    translation ((1/2) *& vec3Z) .*.
                    linear (pointZTo (to &- from)) .*.
                    translation from






        
matrix_basis ::  [Char]
matrix_basis = "matrix_basis"

newFlatTriangleObj
  :: (?sceneE::Python ()) =>
     PythonVar
     -> MeshVertex -> MeshVertex -> MeshVertex -> String -> Python ()
newFlatTriangleObj var p0 p1 p2 name =
    let
        v1 = p1 &- p0
        l1 = norm v1
        u1 = v1&/l1

        v2_0 = p2 &- p0
        a = dotprod u1 v2_0
        v2 = v2_0 &- a *& u1
        l2 = norm v2
        u2 = v2&/l2
    in
        if l1==0 || l2==0
        then
            -- fallback
            newMeshObj var (Mesh name [p0,p1,p2] [(0,1,2)] False [] [])
        else do
            -- use an orthogonal transformation so we get a nice local coordinate system
            newMeshObj var 
                (Mesh name [    zero
                           ,    l1*&vec3X
                           ,    l2 *& vec3Y &+ (a *& vec3X) ] 
                    [(0,1,2)] False [] [])
            applyBlenderTransform var (BProj4
                (translateAfter4 p0 (safeOrthogonal (Mat3 u1 u2 (crossprod u1 u2)))))


                

        


safeOrthogonal :: Mat3 -> Proj4
safeOrthogonal m = 
    assert (matrixApproxEq (m .*. transpose m) idmtx) $
    assert (matrixApproxEq (transpose m .*. m) idmtx) $
    linear m

newTextObj
  :: (?sceneE::Python ()) => PythonVar -> TextCurve -> Python ()
newTextObj var txt = do
    let txtVar = pyv "txt" 
    textCurve_init txtVar txt 
    newObj var (blenderName txt) txtVar

newCurveObj :: (?sceneE::Python ()) => PythonVar -> BlenderCurve -> Python ()
newCurveObj var (cu :: BlenderCurve) = do
    let curveVar = pyv "curve"
    curve_init curveVar cu
    newObj var (blenderName cu) curveVar


applyBlenderTransform :: ToPython a => a -> BlenderTransform -> Python ()
applyBlenderTransform objVar tf =
    case tf of
         LocAndEulers loc (EulerAnglesXYZ x y z) -> do
            objVar <.> "rotation_euler" .= (x,y,z)
            objVar <.> "location" .= loc

         BProj4 m -> objVar <.> "matrix_basis" .= m

bobj_applyTransform :: ToPython a => a -> BlenderObject a1 -> Python ()
bobj_applyTransform objVar = applyBlenderTransform objVar . bobj_transform


newLampObj
  :: (?sceneE::Python ()) => PythonVar -> LampObj -> Python ()
newLampObj objVar BObj{..} = do
    lamp_init lampVar bobj_data
    newObj objVar (lamp_name bobj_data) lampVar
    applyBlenderTransform objVar bobj_transform
    
    where
        lampVar = pyv "lamp"
                



    

-- | Creates a new object with the given name and data, links it to the scene, assigns it to the given var

newObj
  :: (?sceneE::Python (), ToPython t) =>
     PythonVar -> String -> t -> Python ()
newObj (var :: PythonVar) name objData = do
    var .= (methodCallExpr (bpy_dat "objects") "new" (str name,objData))
    methodCall1 (?sceneE <.> "objects") "link" var

newEmptyObj :: (?sceneE::Python ()) => PythonVar -> String -> Python ()
newEmptyObj var name = newObj var name none

--linkObjToScene =

grpLink :: BlenderGroup -> Python () -> Python ()
grpLink gr obj = methodCall1 (bgrp_objectsE gr) "link" obj

newMeshObj :: (?sceneE::Python ()) => PythonVar -> Mesh -> Python ()
newMeshObj var m =
    do 
        let _meshVar = pyv "decoConeMesh"

        mesh_init _meshVar m
        newObj var (meshName m) _meshVar 

        setActiveObject ?sceneE var
        -- don't know how to determine whether we are in editmode already (seems to be determined at random for new objects...), so try/catch
        ln "try:"
        indent $ do
            normals_make_consistent
        ln "except RuntimeError:" 
        indent $ do
            editmode_toggle
            normals_make_consistent
        editmode_toggle



normals_make_consistent :: Python ()
normals_make_consistent = methodCall (bpy_ops "mesh") "normals_make_consistent" ()

originToGeometry :: Python ()
originToGeometry = do 
            methodCall (bpy_ops "object") "origin_set" 
                (SingleArg (namedArg "type" (str "ORIGIN_GEOMETRY")))

setActiveObject :: (ToPython r, ToPython a) => a -> r -> Python ()
setActiveObject scene obj = do
    scene <.> "objects.active" .= obj

editmode_toggle :: Python ()
editmode_toggle = methodCall (bpy_ops "object") "editmode_toggle" ()

deselectAllStmt :: Python ()
deselectAllStmt = foreachStmt "o" (py "context.selected_objects") 
    (\o -> o <.> "select" .= False)


assignRenderSettingsProperties :: ToPython a => a -> RenderSettings -> Python ()
assignRenderSettingsProperties _sceneVar RS{..} = do
    _sceneVar <.> "render.resolution_x" .= render_resolution_x
    _sceneVar <.> "render.resolution_y" .= render_resolution_y
    awhen render_filepath (\fp -> _sceneVar <.> "render.filepath" .= str fp)

setName :: ToPython a => String -> a -> Python ()
setName n obj = obj <.> "name" .= str n

setMaterial :: (ToPython r, ToPython a) => r -> a -> Python ()
setMaterial m obj = obj <.> "active_material" .= m 

quit_blender :: Python ()
quit_blender = methodCall (bpy_ops "wm") "quit_blender" ()

newCamObj :: (?sceneE::Python ()) =>PythonVar -> BlenderObject BlenderCamData -> Python ()
newCamObj objVar BObj{..} = do
    let camVar = pyv "cam"
    blenderInit camVar (bobj_data :: BlenderCamData)
    newObj objVar bobj_name camVar
    applyBlenderTransform objVar bobj_transform



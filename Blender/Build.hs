{-# LANGUAGE ImplicitParams, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
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

newFlatTriangleObj var p0 p1 p2 name =
    newMeshObj var (Mesh name [p0,p1,p2] [(0,1,2)] False [] [])


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
    newObj var (id_name txt) txtVar

newCurveObj var (cu :: BlenderCurve) = do
    let curveVar = pyv "curve"
    curve_init curveVar cu
    newObj var (id_name cu) curveVar



newLampObj
  :: (?sceneE::Python ()) => PythonVar -> LampObj -> Python ()
newLampObj objVar LampObj{..} = do
    lamp_init lampVar lamp_lamp
    newObj objVar (lamp_name lamp_lamp) lampVar
    objVar <.> "rotation_euler" .= lamp_eulers 
    objVar <.> "location" .= lamp_location 

    
    
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

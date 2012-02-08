{-# LANGUAGE CPP, TemplateHaskell, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, DeriveGeneric, ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, GADTs, TypeFamilies, NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Blender.ID where

import ConcreteNormal.PreRenderable
import Data.Function
import Data.List
import Prelude hiding(catch)
import ToPython
import Util
import qualified Data.Foldable as Fold
import Data.Maybe
import Blender.Types
import Blender.Blenderable
import qualified Data.Vector as V
import Data.Vect.Double.Base
import FileLocation
import Control.Monad

type PythonVar = Python ()


-- | Corresponds to the @ID@ class in blender <http://www.blender.org/documentation/blender_python_api_2_59_3/bpy.types.ID.html> (for our purposes, an instance of this class is a thing that has a unique (per scene) name and gets its own python variable)
class BlenderID a where
    id_name :: a -> String

    id_globalVarName :: a -> String

    -- | Create python statements that initialize the given variable according to the value of this @a@.
    id_init :: PythonVar -> a -> Python ()

    id_collectUniqueThings :: PreDeltaSet2 s => Blenderable s -> [a]

id_globalVar :: BlenderID a => a -> Python ()
id_globalVar = py . id_globalVarName

instance BlenderID BlenderImage where
    id_name = bimg_name
    id_globalVarName = ("image_"++) . id_name

    id_init var img = do

        var .= methodCallExpr1 (bpy_dat "images") "load" (str (bimg_filepath img))


    id_collectUniqueThings = mapMaybe (\t -> case t of
                                                  ImgTex {imgtex_img=i} -> Just i
                                                  --_ -> Nothing
                                                  ) . id_collectUniqueThings

instance BlenderID Texture where
    id_name = tex_name
    id_globalVarName = ("texture_"++) . id_name

    id_init var t = do

        var .= methodCallExpr (bpy_dat "textures") "new" (str (tex_name t), str texType)
        typeSpecificStmts

        where

            (texType,typeSpecificStmts) = 
                case t of
                    ImgTex{..} -> ("IMAGE", do 
                        var <.> "image" .= id_globalVar imgtex_img 
                        )



    id_collectUniqueThings = 
                nubOn tex_name . map mts_tex 
                . concatMap ma_textureSlots . id_collectUniqueThings

instance BlenderID Material where
    id_name = ma_name
    id_globalVarName = ("material_"++) . id_name

    id_init var Material{..} = do
        var .= methodCallExpr1 (bpy_dat "materials") "new" (str ma_name)

        mapM_ (\mts -> 
                do
                    let mtsvar = py "textureSlot"
                    mtsvar .= methodCallExpr (var <.> "texture_slots") "add" ()
                    mts_init mtsvar mts

            ) ma_textureSlots

        let allProps = ("use_transparent_shadows" & True)
                            : ma_props
                            ++ Fold.concatMap transparency ma_transparency 

        setProps var allProps

    id_collectUniqueThings = nubOn ma_name . concatMap f . ba_allFaceInfos
        where
            f x = faceMat x
                    : catMaybes [ bfi_helpLineMat x, bfi_labelMat x ]


mts_init :: Python () -> MaterialTextureSlot -> Python ()
mts_init var MTS{..} = do 
    setProps var props

  where
    props = 
            "texture" & id_globalVar mts_tex
        :   "texture_coords" & str "UV"
        :   "uv_layer" & str mts_uv_layer
        :   mts_props 




instance BlenderID BlenderGroup where
    id_name = bgrp_name
    id_globalVarName = ("group_"++) . id_name

    id_init var x = do
        var .= methodCallExpr1 (bpy_dat "groups") "new" (str (bgrp_name x)) 

    id_collectUniqueThings = nubOn bgrp_name . concatMap bfi_groups . ba_allFaceInfos 


bgrp_objectsE :: BlenderGroup -> Python ()
bgrp_objectsE (bgrp :: BlenderGroup) = id_globalVar bgrp <.> "objects" 

id_initGlobalVar :: BlenderID a => a -> Python ()
id_initGlobalVar x = id_init (id_globalVar x) x



instance BlenderID BlenderCurve where
    id_name = curve_name

    id_globalVarName = ("group_"++) . id_name

    id_collectUniqueThings = $undef

    id_init = curve_init
    

appendMaterialsStmt
  :: PythonVar -> [Material] -> Python ()
appendMaterialsStmt var mats = 
    forM_ mats (\m -> methodCall1 (var <.> "materials") "append" (id_globalVar m))
    
curve_init :: PythonVar -> BlenderCurve -> Python ()
curve_init curveVar BlenderCurve{..} = do
        curveVar .= methodCallExpr (bpy_dat "curves") "new" (str curve_name, str "CURVE")
        curveVar <.> "dimensions" .= str "3D" 
        curveVar <.> "bevel_depth" .= bevel_depth
        curveVar <.> "bevel_resolution" .= bevel_resolution
        curveVar <.> "use_fill_front" .= False
        curveVar <.> "use_fill_back" .= False
        curveVar <.> "resolution_u" .= curve_resolution_u
        appendMaterialsStmt curveVar curve_mats 
        --    curveVar <.> "fill_mode" .= str "half"
        
        let splineVar = py "spline"
        mapM_ (spline_init curveVar splineVar) curve_splines


spline_init
  :: PythonVar -> PythonVar -> BlenderSpline -> Python ()
spline_init curveVar splineVar s =
        case s of
            Nurbs{..} -> do


                splineVar .= methodCallExpr1 (curveVar <.> "splines") "new" (str "NURBS")
                methodCall (splineVar <.> "points") "add" 
                    (SingleArg (V.length spline_points-1))
                    -- -1 because the new spline already has one point
                methodCall (splineVar <.> "points") "foreach_set" (str "co",
                    concatMap (asList . extendVec34) (V.toList spline_points))
                splineVar <.> "order_u" .= order_u
                splineVar <.> "use_endpoint_u" .= use_endpoint_u
                splineVar <.> "use_cyclic_u" .= use_cyclic_u
                splineVar <.> "use_smooth" .= True
                splineVar <.> "resolution_u" .= spline_resolution_u
        

extendVec34 :: Vec3 -> Vec4
extendVec34 = extendWith 1


mesh_init :: PythonVar -> Mesh -> Python ()
mesh_init meshVar m = do
        meshVar .= (methodCallExpr1 (bpy_dat "meshes") "new" (str "M"))

        methodCall meshVar "from_pydata" (meshVertices m,emptyList,meshFaces m)

        when (meshSmooth m)
            (foreachStmt "f" (meshVar <.> "faces") (\f ->
                f <.> "use_smooth" .= True))

        forM_ (uv_textures m)
            (\(MeshUVLayer name uvs) -> do
                let meshTextureFaceLayerVar = py "meshTextureFaceLayer"
                meshTextureFaceLayerVar .= methodCallExpr1 (meshVar <.> "uv_textures") "new" (str name)
                methodCall
                    (meshTextureFaceLayerVar <.> "data") 
                    "foreach_set"
                    ( str "uv"
                    , concatMap ((++[-1,-1]) . concatMap asList . asList) uvs ))

        appendMaterialsStmt meshVar (meshMats m)

--             "print('vertices:')",
--             "for x in me.vertices:",
--             "   pprint(x.co)",

        methodCall meshVar "update" ()



-- instance BlenderID (BlenderObject d) where
--     id_name = bobj_name
-- 
--     id_globalVarName = ("object_"++) . id_name
-- 
--     id_collectUniqueThings = $undef
-- 
--     id_init var BObj{..} = do
--         var .= (methodCallExpr (bpy_dat "objects") "new" (str bobj_name, objData))
-- 


textCurve_init :: PythonVar -> TextCurve -> Python ()
textCurve_init txtVar TextCurve{..} = do
    txtVar .= methodCallExpr (bpy_dat "curves") "new" (str textCurve_name, str "FONT")
    txtVar <.> "body" .= str textCurve_text
    txtVar <.> "extrude" .= textCurve_extrude
    txtVar <.> "align" .= str "CENTER"
    appendMaterialsStmt txtVar textCurve_mats 


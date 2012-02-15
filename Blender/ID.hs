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
    id_globalVarName = ma_globalVarName

    id_init var Material{..} = do
        var .= methodCallExpr1 (bpy_dat "materials") "new" (str ma_name)

        mapM_ (\mts -> 
                do
                    let mtsvar = py "textureSlot"
                    mtsvar .= methodCallExpr (var <.> "texture_slots") "add" ()
                    mts_init mtsvar mts

            ) ma_textureSlots

        var <.> "use_transparent_shadows" .= True
        var <.> "specular_hardness" .= ma_specular_hardness
        var <.> "specular_intensity" .= ma_specular_intensity
        var <.> "diffuse_color" .= ma_diffuse_color 

        let allProps =
                               ma_props
                            ++ Fold.concatMap transparency ma_transparency 

        setProps var allProps

    id_collectUniqueThings = nubOn ma_name . concatMap f . ba_allFaceInfos
        where
            f x = faceMat x
                    : catMaybes [ (helpLineMat <=< bfi_helpLineSettings) x, bfi_labelMat x ]


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
    id_name = curve_name . curve_base
    id_globalVarName = ("curve_"++) . id_name
    id_collectUniqueThings = $undef
    id_init = curve_init

instance BlenderID TextCurve where
    id_name = curve_name . textCurve_base
    id_globalVarName = ("textCurve_"++) . id_name
    id_collectUniqueThings = $undef
    id_init = textCurve_init

instance BlenderID BlenderSurface where
    id_name = curve_name . surface_base
    id_globalVarName = ("surface_"++) . id_name
    id_collectUniqueThings = $undef
    id_init = surface_init
    




setSplineDimOpts
  :: ToPython a => a -> [Char] -> SplineDimOpts -> Python ()
setSplineDimOpts splineVar u_or_v SplineDimOpts{..} = do
    splineVar <.> ("order_" ++ u_or_v) .= order
    splineVar <.> ("use_endpoint_" ++ u_or_v) .= use_endpoint
    splineVar <.> ("use_cyclic_" ++ u_or_v) .= use_cyclic
    splineVar <.> ("resolution_" ++ u_or_v) .= spline_resolution


commonSplineInit
  :: 
     PythonVar -> PythonVar -> V.Vector Vec3 -> Python ()
commonSplineInit curveVar splineVar points = do
    splineVar .= methodCallExpr1 (curveVar <.> "splines") "new" (str "NURBS")
    methodCall (splineVar <.> "points") "add" 
        (SingleArg (V.length points-1))
        -- -1 because the new spline already has one point
    methodCall (splineVar <.> "points") "foreach_set" (str "co",
        concatMap (asList . extendVec34) (V.toList points))
    splineVar <.> "use_smooth" .= True

spline_init
  :: PythonVar -> PythonVar -> BlenderSpline -> Python ()
spline_init curveVar splineVar s =
        case s of
            Nurbs{..} -> do
                commonSplineInit curveVar splineVar spline_points 
                setSplineDimOpts splineVar "u" spline_dimOpts
        
spline2d_init
  :: PythonVar -> PythonVar -> BlenderSpline2D -> Python ()
spline2d_init curveVar splineVar s =
        case s of
            Nurbs2D{..} -> do
                commonSplineInit curveVar splineVar spline2d_points 
                splineVar <.> "point_count_u" .= spline2d_point_count_u 
                setSplineDimOpts splineVar "u" spline2d_u
                setSplineDimOpts splineVar "v" spline2d_v
                error ("spline2d: not implemented (can't figure out how to set the dimensions of the control point array, might be impossible via python)")


extendVec34 :: Vec3 -> Vec4
extendVec34 = extendWith 1




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





-- * Curves

curveBase_init :: PythonVar -> BlenderCurveBase -> String -> Python ()
curveBase_init curveVar BlenderCurveBase{..} subclass = do
        curveVar .= methodCallExpr (bpy_dat "curves") "new" (str curve_name, str subclass)
        curveVar <.> "resolution_u" .= curve_resolution_u
        appendMaterialsStmt curveVar curve_mats 
    
curve_init :: PythonVar -> BlenderCurve -> Python ()
curve_init curveVar BlenderCurve{..} = do
        curveBase_init curveVar curve_base "CURVE"
        curveVar <.> "dimensions" .= str "3D" 
        curveVar <.> "bevel_depth" .= bevel_depth
        curveVar <.> "bevel_resolution" .= bevel_resolution
        curveVar <.> "use_fill_front" .= False
        curveVar <.> "use_fill_back" .= False
        --    curveVar <.> "fill_mode" .= str "half"
        
        let splineVar = py "spline"
        mapM_ (spline_init curveVar splineVar) curve_splines

textCurve_init :: PythonVar -> TextCurve -> Python ()
textCurve_init txtVar TextCurve{..} = do
    curveBase_init txtVar textCurve_base "FONT"
    txtVar <.> "body" .= str textCurve_text
    txtVar <.> "extrude" .= textCurve_extrude
    txtVar <.> "align" .= str "CENTER"


surface_init :: PythonVar -> BlenderSurface -> Python ()
surface_init var BlenderSurface{..} = do
    curveBase_init var surface_base "SURFACE"



lamp_init :: PythonVar -> BlenderLamp -> Python ()
lamp_init lightVar l =
    case l of
         Sun{..} -> do

            lightVar .= methodCallExpr (bpy_dat "lamps") "new" (str lamp_name, str "SUN") 
            lightVar <.> "shadow_method" .= str "RAY_SHADOW"

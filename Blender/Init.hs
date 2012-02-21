{-# LANGUAGE ImplicitParams, TemplateHaskell, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, DeriveGeneric, ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, GADTs, TypeFamilies, NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Blender.Init where

import ConcreteNormal.PreRenderable
import Data.Function
import Data.List
import Prelude hiding(catch)
import ToPython
import qualified Data.Foldable as Fold
import Blender.Types
import Blender.Blenderable
import qualified Data.Vector as V
import Data.Vect.Double.Base
import Blender.AsGlobal
import Data.Proxy


class BlenderInit a where
    -- | Create python statements that initialize the given variable according to the value of this @a@.
    blenderInit :: PythonVar -> a -> Python ()

instance BlenderInit BlenderImage where

    blenderInit var img = do

        var .= methodCallExpr1 (bpy_dat "images") "load" (str (bimg_filepath img))



instance BlenderInit Texture where

    blenderInit var t = do

        var .= methodCallExpr (bpy_dat "textures") "new" (str (tex_name t), str texType)
        typeSpecificStmts

        where

            (texType,typeSpecificStmts) = 
                case t of
                    ImgTex{..} -> ("IMAGE", do 
                        var <.> "image" .= blenderGlobalVarFor imgtex_img 
                        )






instance BlenderInit Material where

    blenderInit var Material{..} = do
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
        var <.> "ambient" .= ma_ambient 

        let allProps =
                               ma_props
                            ++ Fold.concatMap transparency ma_transparency 

        setProps var allProps

instance BlenderInit BlenderCurve where
    blenderInit = curve_init

instance BlenderInit TextCurve where
    blenderInit = textCurve_init

instance BlenderInit BlenderSurface where
    blenderInit = surface_init
    
instance BlenderInit BlenderLamp where
    blenderInit = lamp_init

transparency :: TransparencySettings -> [([Char], Python ())]
transparency Trans{..} =
                [
                "use_transparency" & True,
                "transparency_method" & str "RAYTRACE",
                -- "transparency_method" & "'Z_TRANSPARENCY'",
                "alpha" & (_alpha),
                "specular_alpha" & (_spec_alpha),
                "translucency" & _translucency,
                "raytrace_transparency.fresnel" & (_fresnel),
                "raytrace_transparency.fresnel_factor" & _fresnel_factor,
                "raytrace_transparency.depth" & (15::Int)
                ]



mts_init :: Python () -> MaterialTextureSlot -> Python ()
mts_init var MTS{..} = do 
    setProps var props

  where
    props = 
            "texture" & blenderGlobalVarFor mts_tex
        :   "texture_coords" & str "UV"
        :   "uv_layer" & str mts_uv_layer
        :   mts_props 




instance BlenderInit BlenderGroup where

    blenderInit var x = do
        var .= methodCallExpr1 (bpy_dat "groups") "new" (str (bgrp_name x)) 



bgrp_objectsE :: BlenderGroup -> Python ()
bgrp_objectsE (bgrp :: BlenderGroup) = blenderGlobalVarFor bgrp <.> "objects" 








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

blenderInitGlobalVar
  :: (BlenderifiesAsGlobal a, BlenderInit a) => a -> Python ()
blenderInitGlobalVar x = blenderInit (blenderGlobalVarFor x) x

blenderInitGlobalVars :: (BlenderInit a, BlenderifiesAsGlobal a, PreDeltaSet2 s) =>
     Scene s -> Proxy a -> Python ()
blenderInitGlobalVars scene p =
    mapM_ blenderInitGlobalVar (collectUniqueThingsFromScene scene `asProxyTypeOf` fmap (:[]) p)



instance BlenderInit BlenderCamData where
    blenderInit camVar BCamD{..} = do
                camVar .= methodCallExpr1 (bpy_dat "cameras") "new" (str bcamd_name)
                camVar <.> "angle" .= bcamd_FOV


instance BlenderInit BlenderWorld where
    blenderInit worldVar BWorld{..} = do
        worldVar .= methodCallExpr1 (bpy_dat "worlds") "new" (str "TheWorld")

        initLightSettings worldVar bworld_light


        mapM_ (setProp' worldVar) bworld_otherProps




initLightSettings
  :: ToPython a => a -> WorldLightSettings -> Python ()
initLightSettings worldVar WorldLightSettings{..} = do

    () {- type inference will fail without this; not considered a bug
            http://hackage.haskell.org/trac/ghc/ticket/5678
            -}
            <- case wls_gatherMethod of
        GatherNone -> do
            return ()
        GatherApprox -> do
            worldVar <.> "light_settings.gather_method" .= str "APPROXIMATE"
        GatherRaytrace -> do
            worldVar <.> "light_settings.gather_method" .= str "RAYTRACE"

    case wls_envLight of
        NoEnvLight -> 
            worldVar <.> "light_settings.use_environment_light" .= False
        
        EnvLight ener -> do
            worldVar <.> "light_settings.use_environment_light" .= True
            worldVar <.> "light_settings.environment_energy" .= ener

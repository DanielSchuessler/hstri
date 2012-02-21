{-# LANGUAGE CPP, TemplateHaskell, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, DeriveGeneric, ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, GADTs, TypeFamilies, NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Blender.AsGlobal where

import ConcreteNormal.PreRenderable
import Data.Function
import Data.List
import Prelude hiding(catch)
import ToPython
import Util
import Data.Maybe
import Blender.Types
import Blender.Blenderable
import Control.Monad

class BlenderNamed a where
    blenderName :: a -> String 

instance BlenderNamed (BlenderObject a) where
    blenderName = bobj_name
    
instance BlenderNamed BlenderCamData where
    blenderName = bcamd_name

instance BlenderNamed BlenderCurve where
    blenderName = curve_name . curve_base

instance BlenderNamed TextCurve where
    blenderName = curve_name . textCurve_base

instance BlenderNamed BlenderSurface where
    blenderName = curve_name . surface_base

instance BlenderNamed BlenderLamp where
    blenderName = lamp_name

instance BlenderNamed BlenderImage where
    blenderName = bimg_name

instance BlenderNamed Texture where
    blenderName = tex_name

instance BlenderNamed Material where
    blenderName = ma_name

instance BlenderNamed BlenderGroup where
    blenderName = bgrp_name

-- | Corresponds to the @ID@ class in blender <http://www.blender.org/documentation/blender_python_api_2_59_3/bpy.types.ID.html> (for our purposes, an instance of this class is a thing that has a unique (per scene) name and gets its own python variable)
class BlenderifiesAsGlobal a where
    blenderGlobalVarName :: a -> String

    collectUniqueThingsFromScene :: PreDeltaSet2 s => Scene s -> [a]

blenderGlobalVarFor :: BlenderifiesAsGlobal a => a -> Python ()
blenderGlobalVarFor = py . blenderGlobalVarName


instance BlenderifiesAsGlobal BlenderImage where
    blenderGlobalVarName = ("image_"++) . bimg_name



    collectUniqueThingsFromScene = mapMaybe (\t -> case t of
                                                  ImgTex {imgtex_img=i} -> Just i
                                                  --_ -> Nothing
                                                  ) . collectUniqueThingsFromScene


instance BlenderifiesAsGlobal Texture where
    blenderGlobalVarName = ("texture_"++) . blenderName

    collectUniqueThingsFromScene = 
                nubOn tex_name . map mts_tex 
                . concatMap ma_textureSlots . collectUniqueThingsFromScene




instance BlenderifiesAsGlobal Material where
    blenderGlobalVarName = ma_globalVarName

    collectUniqueThingsFromScene = nubOn ma_name . concatMap f . ba_allFaceInfos . scene_blenderable
        where
            f x = faceMat x
                    : catMaybes [ (helpLineMat <=< bfi_helpLineSettings) x
                                , bfi_labelMat x 
                                , bfi_labelMat2 x 
                                ]








instance BlenderifiesAsGlobal BlenderGroup where
    blenderGlobalVarName = ("group_"++) . blenderName


    collectUniqueThingsFromScene = 
        nubOn bgrp_name . concatMap bfi_groups . ba_allFaceInfos . scene_blenderable
        




{-# LANGUAGE CPP, TemplateHaskell, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, DeriveGeneric, ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, GADTs, TypeFamilies, NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Blender.ID(
    module Blender.Types,
    module Blender.AsGlobal,
    module Blender.Init,
    BlenderID
 )where

import Blender.Types
import Blender.Init
import Blender.AsGlobal



-- | Corresponds to the @ID@ class in blender <http://www.blender.org/documentation/blender_python_api_2_59_3/bpy.types.ID.html> (for our purposes, an instance of this class is a thing that has a unique (per scene) name and gets its own python variable)
--
-- This class is currently (since factoring things out) rather pointless
class (BlenderNamed a, BlenderInit a) => BlenderID a where


instance BlenderID BlenderImage where
instance BlenderID Texture where
instance BlenderID Material where
instance BlenderID BlenderGroup where
instance BlenderID BlenderCurve where
instance BlenderID TextCurve where
instance BlenderID BlenderSurface where
instance BlenderID BlenderLamp where



{-# LANGUAGE CPP, TemplateHaskell, FunctionalDependencies, StandaloneDeriving, FlexibleContexts, FlexibleInstances, DeriveGeneric, ScopedTypeVariables, Rank2Types, NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses, GADTs, TypeFamilies, NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Blender.Mesh where

import ConcreteNormal.PreRenderable
import Data.Function
import Data.List
import Prelude hiding(catch)
import ToPython
import Blender.Types
import Blender.Blenderable
import Control.Monad
import Data.Numbering
import Data.Vect.Double.Base(Vec2(..),Vec3(..))



data Mesh = Mesh {
        meshName :: String,
        meshVertices :: [MeshVertex],
        meshFaces :: [Triple MeshVertexIndex],
        meshSmooth :: Bool,
        uv_textures :: [MeshUVLayer],
        meshMats :: [Material]
    }

type UV = Vec2

-- | Aka @MeshTextureFaceLayer@
data MeshUVLayer = MeshUVLayer UVLayerName [Triple UV] 

type MeshVertexIndex = Int
type MeshVertex = Vec3

mesh :: Numbering v -> (v -> MeshVertex) -> [Triple v] -> Mesh
mesh vertNu vertCoords faces = Mesh {
        meshName = "Anonymous mesh",
        meshVertices = map vertCoords (nuElements vertNu),
        meshFaces = map (map3 (toInt vertNu)) faces,
        meshSmooth = True,
        uv_textures = [],
        meshMats = []
    }



mesh_setSmooth :: ToPython a => a -> Python ()
mesh_setSmooth meshE =
            (foreachStmt "f" (meshE <.> "faces") (\f ->
                f <.> "use_smooth" .= True))

mesh_init :: PythonVar -> Mesh -> Python ()
mesh_init meshVar m = do
        meshVar .= (methodCallExpr1 (bpy_dat "meshes") "new" (str (meshName m)))

        methodCall meshVar "from_pydata" (meshVertices m,emptyList,meshFaces m)

        when (meshSmooth m) (mesh_setSmooth meshVar)

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

        methodCall meshVar "update" (SingleArg True)


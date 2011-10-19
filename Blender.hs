{-# LANGUAGE FlexibleContexts, TemplateHaskell, ScopedTypeVariables, PolymorphicComponents, RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 
module Blender where

import Control.Arrow
import Control.Monad.Writer hiding(sequence_,mapM_)
import Data.Foldable
import Data.List
import Data.NaturalNumber
import Data.NaturalNumber
import Data.Vect.Double
import Data.Vect.Double.GramSchmidt
import Debug.Trace
import Prelude hiding(catch,mapM_,sequence_) 
import System.Environment
import System.Process
import Text.Printf.TH
import DeltaSet
import TypeLevel.NaturalNumber hiding(NaturalNumber)

parens x = "("++x++")"
showTuple = parens . intercalate ", "

class BlenderLabel a where
    blenderLabel :: a -> String

-- instance (BlenderLabel a, BlenderLabel b) => BlenderLabel (DisjointUnion a b) where
--     blenderLabel (DisjointUnion a b) n = blenderLabel a n ||| blenderLabel b n 

data MaterialKind = TriangulationMat | NormalSurfMat
    deriving (Show,Enum,Bounded)

data VertexInfo = VertexInfo {
    vertexInfoCoords :: Vec3
}


data Scene t = Scene {
    sceneTriang :: t
}

class VertexInfoClass v where
    vertexInfo :: v -> VertexInfo

coords = vertexInfoCoords . vertexInfo 

instance (VertexInfoClass v1, VertexInfoClass v2) => VertexInfoClass (Either v1 v2) where
    vertexInfo = vertexInfo ||| vertexInfo 

matName :: MaterialKind -> Int -> String
matName k i = show k ++ "Mat"++show i 

setProp :: String -> String -> String -> Blender ()
setProp obj p v = ln $ $(printf "%s.%s = %s") obj p v 

-- | Add a line to the blender script
ln :: String -> Blender ()
ln x = tell (x++"\n")

lns :: [String] -> Blender ()
lns = mapM_ ln

type Blender = Writer String

matSetDiffuseColor :: String -> (Double,Double,Double) -> Blender ()
matSetDiffuseColor m (r, g, b) = setProp m "diffuse_color" ($(printf "(%f,%f,%f)") r g b)


matSetTransp mat alpha spec_alpha =
             mapM_ (uncurry $ setProp mat) [
                ("use_transparency","True"),
                ("transparency_method","'RAYTRACE'"),
                -- ("transparency_method","'Z_TRANSPARENCY'"),
                ("alpha",show alpha),
                ("specular_alpha",show spec_alpha),
                ("translucency","0.8"),
                ("raytrace_transparency.fresnel","1")
                ]

-- toBlender :: forall a. (BlenderLabel a, 
--                         VertexInfoClass (Vertex a),
--                         Show (Vertex a), 
--                         Show (Edge a), 
--                         Show (Triangle a), 
--                         DeltaSet a) => 
--        Scene a
--     -> Blender ()
toBlender Scene{..} = result
    where
        result = do
            ln "from bpy import *"
            ln "from pprint import pprint"
            materialDefs
            mapM_ handleV (allVertices sceneTriang)
            mapM_ handleE (allEdges sceneTriang)
            mapM_ handleT (allTriangles sceneTriang)


        facMatVar = matName TriangulationMat 2

        dims = [0..2]


        materialDefs :: Blender ()
        materialDefs = do
             sequence_ [ newMaterial name name | k <- [minBound .. maxBound], i <- dims, 
                                            let name = matName k i ]


             matSetTransp facMatVar 0.1 0.3 
             matSetTransp (matName NormalSurfMat 2) 0.2 0.35

             sequence_ [ matSetDiffuseColor (matName NormalSurfMat i) 
                            (0,0.2,1)
             
                            | i <- dims ]



        handleV v = do
                sphere coords vertexRadius
                objSetName (blenderLabel v)
                objSetMaterial (matName vertexInfoKind 0)
            
            where
                VertexInfo {..} = vertexInfo v 

        handleE e = do
                cylinder (coords v0) (coords v1) edgeRadius
                objSetName (blenderLabel e)
                objSetMaterial (matName (vertexInfoKind v0)1)
            

            where
                (v1,v0) = edgeVertices sceneTriang e


        handleT t = do
                    triangle (coords v0) (coords v1) (coords v2)
                    objSetName (blenderLabel t)
                    objSetMaterial (matName (vertexInfoKind v0) 2)
                    objSetProp "show_transparent" "True"

            where
                (v2,v1,v0) = triangleVertices sceneTriang t
        
        objSetName :: String -> Blender ()
        objSetName = objSetProp "name" . show

        objSetMaterial = objSetProp "active_material"

        objSetProp = setProp "obj"

        
        sphere loc radius = do
            ln $ $(printf "ops.surface.primitive_nurbs_surface_sphere_add(location=%s)")
                (pyVec3 loc)

            assignObjFromContext

            ln $ $(printf "obj.scale = %s") (pyVec3 (Vec3 radius radius radius))


        cylinder :: Vec3 -> Vec3 -> Double -> Blender ()
        cylinder from to radius = do
            ln "ops.surface.primitive_nurbs_surface_cylinder_add()"

            assignObjFromContext

            ln $ $(printf "obj.matrix_basis = %s") (pyProj4 m)

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

        triangle p0 p1 p2 = mesh [p0,p1,p2] "(0,1,2)"








        vertexRadius = 0.5
        edgeRadius = 0.12
        
        assignObjFromContext = assignObj "context.object"
        assignObj x = ln $ "obj = " ++ x

        newMaterial :: String -> String -> Blender ()
        newMaterial varName name = do
                ln $ $(printf "%s = data.materials.new(%H)") varName name 
                ln $ $(printf "%s.use_transparent_shadows = True") varName


testBlender s = do
    let fn = "/tmp/foo.py"
    writeFile fn (execWriter $ toBlender s)
    putStrLn fn
    args <- getArgs
    rawSystem "blender" ("-P":fn:args)


anyOrth (Vec3 0 y z) = Vec3 0 (-z) y
anyOrth (Vec3 x y _) = Vec3 (-y) x 0

mesh verts faces = 
            do 
            ln "me = data.meshes.new(\"TriMesh\")"         

            ln $ $(printf "me.from_pydata([%s],[],[%s])") 

                (intercalate ", " $ [ pyVec3 p | p <- verts ])
                faces


--             "print('vertices:')",
--             "for x in me.vertices:",
--             "   pprint(x.co)",

            ln "me.update()"
            ln "obj = data.objects.new(\"Tri\",me)"
            ln "scene = context.scene"
            ln "scene.objects.link(obj)"


pyVec3 (Vec3 x y z) = show (x,y,z)
pyVec4 (Vec4 x y z h) = show (x,y,z,h)
pyMat4 (Mat4 a b c d) = showTuple (fmap pyVec4 [a,b,c,d]) 
pyProj4 = pyMat4 . fromProjective

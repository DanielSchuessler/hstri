{-# LANGUAGE GADTs, NamedFieldPuns, FlexibleContexts, TemplateHaskell, ScopedTypeVariables, PolymorphicComponents, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, CPP, GeneralizedNewtypeDeriving, TypeFamilies, DefaultSignatures, ExtendedDefaultRules, StandaloneDeriving #-} 
{-# OPTIONS -Wall #-}
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
import Data.Vect.Double hiding((*.),(.*.))
import HomogenousTuples
import MathUtil
import PreRenderable
import Prelude hiding(catch,mapM_,sequence_) 
import Simplicial.DeltaSet
import System.Environment
import System.Process
import ToPython
import FaceClasses
import System.Exit
import Simplicial.AnySimplex
import THUtil
import PrettyUtil(Pretty)

#define CONSTRAINTS(a) Pretty (Vert a), ShowN a, Pretty (Arc a)


sceneVar,worldVar,camVar,objVar,lightVar :: Python ()

sceneVar = py "scene"
worldVar = py "world"
camVar = py "cam"
objVar = py "obj"
lightVar = py "light"



objSetName ::  String -> Python ()
objSetName x = objVar <.> "name" .= str x

objSetMaterial ::  ToPython r => r -> Python ()
objSetMaterial x = objVar <.> "active_material" .= x 

ops ::  String -> Python ()
ops x = py "ops" <.> x
dat ::  String -> Python ()
dat x = py "data" <.> x

bpy_props ::  Python ()
bpy_props = py "props"
types ::  String -> Python ()
types x = py "types" <.> x

ma_var ::  Material -> Python ()
ma_var = py . ma_name

triLabelMat ::  Material
triLabelMat = Material "triLbl" [ let r = 0.015 in diffuseColor (r,r,r) ]


objCommon ::  Python () -> String -> Material -> Python ()
objCommon grp faceName faceMat = do
                objSetName faceName
                objSetMaterial (ma_var faceMat)
                grpLink grp objVar
                objVar <.> "simplexlabel" .= str faceName

-- toBlender :: forall a. (BlenderLabel a, 
--                         VertexInfoClass (Vertex a),
--                         Show (Vertex a), 
--                         Show (Edge a), 
--                         Show (blenderTriangle a), 
--                         DeltaSet a) => 
--        Scene a
--     -> Python ()

returnContextStmt :: Python ()
returnContextStmt = py "return context.object"

makeSphereFunDef :: PythonFunDef
makeSphereFunDef = PythonFunDef {
    pfd_name = "makeSphere",
    pfd_argList = ["loc"],
    pfd_defBody =
        do
                methodCall1 (ops "surface") "primitive_nurbs_surface_sphere_add" 
                    (namedArg "location" (py "loc"))
                returnContextStmt
}


makeCylinderFunDef :: PythonFunDef
makeCylinderFunDef = PythonFunDef {
    pfd_name = "makeCylinder",
    pfd_argList = [],
    pfd_defBody = do
            methodCall (ops "surface") "primitive_nurbs_surface_cylinder_add" ()
            returnContextStmt

}

toBlender :: (CONSTRAINTS(a)) => Scene a -> Python ()
toBlender Scene{
        scene_worldProps,
        scene_blenderable = ba@Blenderable{..},
        scene_cams} = result
    where
        deltaSet = ba_ds

        result = do
            ln "from bpy import *"
            ln "from pprint import pprint"

            mapM_ pfd_def [makeSphereFunDef,makeCylinderFunDef]


            -- Make a new scene
            sceneVar .= methodCallExpr1 (dat "scenes") "new" (str "TheScene")
            -- Remove all other scenes
            ln  "for s in data.scenes:"
            indent $ do
                ln ("if (s != "++(renderPython sceneVar)++"):" )
                indent $ do
                    ln  "data.scenes.remove(s)" 

            -- Make a new world, assign it to the scene, set world properties
            worldVar .= methodCallExpr1 (dat "worlds") "new" (str "TheWorld")
            mapM_ (setProp' worldVar) scene_worldProps
            setProp sceneVar "world" worldVar

            -- cameras
            forM_ (zip [0::Integer ..] scene_cams) (\(i, (Cam pos eulers fov)) -> do
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


            mapM_ (handleV ba grp0) (vertices deltaSet)
            mapM_ (handleE ba grp1) (edges deltaSet)
            mapM_ (handleT ba grp2 grpTriLabels) (triangles deltaSet)

            --ln "ops.view3d.viewnumpad(type='CAMERA')"

        


        materialDefs = mapM_ materialToBlender (triLabelMat : ba_materials ) 





handleSimplex
  :: Nat n =>
     Python () -> Blenderable a -> BlenderGroup -> a n -> Python ()
handleSimplex f ba grp si = when (ba_visible ba asi) $ do
        f
        objCommon grp faceName faceMat
    

    where
        asi = AnySimplex si
        FaceInfo{..} = ba_faceInfo ba asi



type BlenderGroup = Python ()

handleV ::  Blenderable a -> BlenderGroup -> Vert a -> Python ()
handleV ba grp v = handleSimplex    
                    (sphere (ba_coords ba v) (ba_vertexThickness ba v)) 
                    ba grp v

handleE :: (CONSTRAINTS(a)) => Blenderable a -> BlenderGroup -> Arc a -> Python ()
handleE ba grp e = handleSimplex 
                    (either handleErr id
                        (cylinder (ba_coords ba v0) (ba_coords ba v1) (ba_edgeThickness ba e)))
                    ba grp e
    

    where
        (v1,v0) = faces10 (ba_ds ba) e
        handleErr err = error (err++"\n"++"In handleE\n"++ $(showExps ['ba,'e]))


handleT :: Blenderable a -> BlenderGroup -> BlenderGroup -> Tri a -> Python ()
handleT ba grp grpTriLabels t = when (ba_visible ba (AnySimplex t)) $ do
            blenderTriangle cv0 cv1 cv2 
            objVar <.> "show_transparent" .= True
            objCommon grp faceName faceMat

            handleTriLabel ba grpTriLabels t

    where
        vs = faces20Ascending (ba_ds ba) t
        (cv0,cv1,cv2) = map3 (ba_coords ba) vs

        FaceInfo{..} = ba_faceInfo ba (AnySimplex t)

        
handleTriLabel :: Blenderable a -> BlenderGroup -> Tri a -> Python ()
handleTriLabel ba grpTriLabels t =
            case ba_triangleLabel ba t of
                Nothing -> return ()

                Just (TriangleLabel lblstr g upDisplacementFactor) -> 
                    let (leftpoint,rightpoint,toppoint) = cvs *. g
                        rightunit = normalize (rightpoint &- leftpoint)
                        basepoint = interpolate 0.5 leftpoint rightpoint
                        upvect = let up0 = (toppoint &- basepoint) 
                                 in up0 &- (dotprod rightunit up0) *& rightunit
                        height = norm upvect
                        upunit = upvect &* recip height 
                        m = 
                            scalingUniformProj4 (0.4 * height)
                            .*.
                            safeOrthogonal (Mat3 rightunit upunit (crossprod rightunit upunit)) 
                            .*.
                            translation (basepoint &+ upDisplacementFactor *& upvect)



                    in do
                        newTextObj lblstr
                        objVar <.> matrix_basis .= m
                        objCommon grpTriLabels (lblstr ++ " on "++show g ++" "++faceName) triLabelMat 


    where
        vs = faces20Ascending (ba_ds ba) t
        cvs = map3 (ba_coords ba) vs

        FaceInfo{..} = ba_faceInfo ba (AnySimplex t)


        
sphere :: Vec3 -> Double -> Python ()
sphere loc radius = do
            objVar .= pfd_call1 makeSphereFunDef loc
            objVar <.> "scale" .= (Vec3 radius radius radius)




cylinder :: Vec3 -> Vec3 -> Double -> Either String (Python ())
cylinder from to radius = 
    if normsqr (from &- to) <= 1E-14
       then Left ("cylinder: from = to") 
       else Right $

    
        do
            objVar .= pfd_call makeCylinderFunDef ()

            objVar <.> matrix_basis .= m

            where
                m :: Proj4
                m = 
                    
                    scaling (Vec3 radius radius (1/2)) .*. 
                    translation ((1/2) *& vec3Z) .*.
                    linear (pointZTo (to &- from)) .*.
                    translation from






        
matrix_basis ::  [Char]
matrix_basis = "matrix_basis"

blenderTriangle ::  Vec3 -> Vec3 -> Vec3 -> Python ()
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


testBlender :: (CONSTRAINTS(a)) => Scene a -> IO ExitCode
testBlender s = do
    let fn = "/tmp/foo.py"
    writeFile fn (renderPython $ toBlender s)
    putStrLn fn
    args <- getArgs
    rawSystem "blender" ("-P":fn:args)


meshVar ::  Python ()
meshVar = py "me"

emptyList ::  [()]
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

txtVar ::  Python ()
txtVar = py "txt"

textThickness :: Double
textThickness = 1E-5

newTextObj ::  String -> Python ()
newTextObj txt = do
    txtVar .= methodCallExpr (dat "curves") "new" (str "T", str "FONT")
    txtVar <.> "body" .= str txt
    txtVar <.> "extrude" .= textThickness
    txtVar <.> "align" .= str "CENTER"
    newObj "T" txtVar


-- | Returns the object in the 'objVar' \"register\" :-)
newObj ::  ToPython t => String -> t -> Python ()
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


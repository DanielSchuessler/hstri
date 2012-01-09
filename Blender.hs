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
import System.Environment
import System.Process
import ToPython
import FaceClasses
import System.Exit
import THUtil
import PrettyUtil(Pretty)
import Simplicial.DeltaSet2

#define CONSTRAINTS(s) Pretty (Vert s), Pretty (Arc s), Pretty (Tri s), DeltaSet2 s, Pretty s


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

-- toBlender :: forall a. (BlenderLabel s, 
--                         VertexInfoClass (Vertex s),
--                         Show (Vertex s), 
--                         Show (Edge s), 
--                         Show (blenderTriangle s), 
--                         DeltaSet s) => 
--        Scene s
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

toBlender :: (CONSTRAINTS(s)) => Scene s -> Python ()
toBlender Scene{
        scene_worldProps,
        scene_blenderable = ba@Blenderable{..},
        scene_cams} = result
    where
        result = do
            ln "from bpy import *"
            ln "from pprint import pprint"

            mapM_ pfd_def [makeSphereFunDef,makeCylinderFunDef]


            -- Make s new scene
            sceneVar .= methodCallExpr1 (dat "scenes") "new" (str "TheScene")
            -- Remove all other scenes
            ln  "for s in data.scenes:"
            indent $ do
                ln ("if (s != "++(renderPython sceneVar)++"):" )
                indent $ do
                    ln  "data.scenes.remove(s)" 

            -- Make s new world, assign it to the scene, set world properties
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


            mapM_ (handleV ba grp0) (vertexList ba_ds)
            mapM_ (handleE ba grp1) (edgeList ba_ds)
            mapM_ (handleT ba grp2 grpTriLabels) (triangleList ba_ds)

            --ln "ops.view3d.viewnumpad(type='CAMERA')"

        


        materialDefs = mapM_ materialToBlender (triLabelMat : ba_materials ) 





handleSimplex
  :: (a -> AnySimplex2Of s) ->
     Python () -> Blenderable s -> BlenderGroup -> a -> Python ()
handleSimplex mkAsi f ba grp si = when (ba_visible ba asi) $ do
        f
        objCommon grp faceName faceMat
    

    where
        asi = mkAsi si
        FaceInfo{..} = ba_faceInfo ba asi



type BlenderGroup = Python ()

handleV ::  Blenderable s -> BlenderGroup -> Vert s -> Python ()
handleV ba grp v = handleSimplex vertToAnySimplex2 
                    (sphere (ba_coords ba v) (ba_vertexThickness ba v)) 
                    ba grp v

handleE :: (CONSTRAINTS(s)) => Blenderable s -> BlenderGroup -> Arc s -> Python ()
handleE ba grp e = handleSimplex edToAnySimplex2
                    (either handleErr id
                        (cylinder (ba_coords ba v0) (ba_coords ba v1) (ba_edgeThickness ba e)))
                    ba grp e
    

    where
        (v1,v0) = faces10 (ba_ds ba) e
        handleErr err = error (err++"\n"++"In handleE\n"++ $(showExps ['ba,'e]))


handleT :: (CONSTRAINTS(s)) => Blenderable s -> BlenderGroup -> BlenderGroup -> Tri s -> Python ()
handleT ba grp grpTriLabels t = when (ba_visible ba (triToAnySimplex2 t)) $ do
            blenderTriangle cv0 cv1 cv2 
            objVar <.> "show_transparent" .= True
            objCommon grp faceName faceMat

            handleTriLabel ba grpTriLabels t

    where
        vs = faces20Ascending (ba_ds ba) t
        (cv0,cv1,cv2) = map3 (ba_coords ba) vs

        FaceInfo{..} = ba_faceInfo ba (triToAnySimplex2 t)

        
handleTriLabel :: (CONSTRAINTS(s)) => Blenderable s -> BlenderGroup -> Tri s -> Python ()
handleTriLabel ba grpTriLabels t =
            case ba_triangleLabel ba t of
                Nothing -> return ()

                Just tl -> 
                    let
                        text = tl_text tl
                        perm = tl_transform tl

                    in let 
                        (leftpoint,rightpoint,toppoint) = cvs *. perm

                        rightvect = rightpoint &- leftpoint
                        rightunit = normalize rightvect

                        upvect = let up0 = (toppoint &- leftpoint) 
                                 in up0 &- (dotprod rightunit up0 *& rightunit)

                        upunit = normalize upvect

                    in let
                        
                        -- | Horizontal center point at the height of the text baseline
                        textCenterBase =
                                let
                                    baryc = (leftpoint &+ rightpoint &+ toppoint)&/3
                                    barybase = baryc &+ 
                                                (dotprod (leftpoint &- baryc) upunit *& upunit) 
                                in
                                    barybase 
                                        &+ tl_up tl *& upvect
                                        &+ tl_rightdispl tl *& rightvect 
                                    



                        thescale = 
                            (
                                        let
                                            r = norm rightvect
                                        in
                                            1.2 * incircleRadius r 
                                                        (norm (toppoint-leftpoint))
                                                        (norm (toppoint-rightpoint))
                                    )
                                
                                * tl_scale tl

                        m = -- $(traceExps "tri" ['t,'text,'textCenterBase,'thescale]) $ 

                            scalingUniformProj4 thescale 
                            .*.
                            safeOrthogonal (Mat3 rightunit upunit (crossprod rightunit upunit)) 
                            .*.
                            translation textCenterBase



                    in do
                        newTextObj text
                        objVar <.> matrix_basis .= m
                        objCommon grpTriLabels (text ++ " on "++show perm++" "++faceName) triLabelMat 


    where
        vs = faces20Ascending (ba_ds ba) t
        cvs = map3 (ba_coords ba) vs

        FaceInfo{..} = ba_faceInfo ba (triToAnySimplex2 t)


        
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


testBlender :: (CONSTRAINTS(s)) => Scene s -> IO ExitCode
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


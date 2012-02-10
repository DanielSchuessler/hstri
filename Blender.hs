{-# LANGUAGE RankNTypes, ImplicitParams, GADTs, NamedFieldPuns, FlexibleContexts, TemplateHaskell, ScopedTypeVariables, PolymorphicComponents, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, CPP, GeneralizedNewtypeDeriving, TypeFamilies, DefaultSignatures, ExtendedDefaultRules, StandaloneDeriving #-} 
{-# OPTIONS -Wall -fwarn-missing-local-sigs -fno-warn-unused-imports #-}
module Blender(
    module Blender.Types,
    module Blender.ID,
    module Blender.Conversion,
    module Blender.Blenderable,
    module MathUtil,
    testBlender,
    toBlender) where

import Blender.Blenderable
import Blender.Conversion
import Blender.ID
import Blender.Types
import Control.Exception
import Control.Monad
import Data.Function
import Data.Maybe
import Data.MemoTrie
import Data.Monoid
import Data.Numbering
import Data.Vect.Double hiding((*.),(.*.))
import MathUtil
import Prelude hiding(catch,mapM_,sequence_) 
import System.Environment
import System.Exit
import System.Process
import THUtil
import ToPython
import Util
import qualified Data.Vector as V
import Data.Time.Clock
import Data.Lens.Common
import Control.Monad.IfElse
import Data.VectorSpace((*^))
import Numeric.AD.Classes(lift)
import Numeric.AD.Vector
import Numeric.AD.Mode.Mixed(diffF)
import Data.Cross(cross3)
import Data.AdditiveGroup((^+^))
import Data.VectorSpace((^*))
import PrettyUtil(docToString)
import PrettyUtil(prettyEqs)
import PrettyUtil(pretty)

#define CONSTRAINTS0(s) Show (Vert s), Show (Ed s), Show (Tri s), Pretty (Vert s), Pretty (Ed s), Pretty (Tri s), PreDeltaSet2 s, Pretty s, Ord (Ed s), Ord (Vert s), Ord (Tri s)
#define CONSTRAINTS(s) ?scene::Scene s, CONSTRAINTS0(s) 

helpLineThickness :: Double
helpLineThickness = 0.001 :: Double

helpLineN :: Int
helpLineN = 19 :: Int

helpLineMaxSteps :: Int
helpLineMaxSteps = 100 :: Int

decoStretchFraction :: Fractional a => a
decoStretchFraction = 0.25

decoConeLengthFraction :: Fractional a => a
decoConeLengthFraction = 0.11 

decoConeWidth :: Fractional a => a
decoConeWidth = 0.04
                



toBeSelectedVar,sceneVar,worldVar,camVar,objVar,lightVar,curveVar :: Python ()

sceneVar = py "scene"
worldVar = py "world"
camVar = py "cam"
objVar = py "obj"
lightVar = py "light"
curveVar = py "curve"
toBeSelectedVar = py "toBeSelected"


objSetName ::  String -> Python ()
objSetName x = objVar <.> "name" .= str x

objSetMaterial ::  ToPython r => r -> Python ()
objSetMaterial x = objVar <.> "active_material" .= x 


ma_var ::  Material -> Python ()
ma_var = id_globalVar




longLabelPropertyName :: [Char]
longLabelPropertyName = "longlabel"

objCommon :: (?scene::Scene s) => [BlenderGroup] -> String -> Maybe Material -> Python ()
objCommon grps faceName faceMatMay = do
                objSetName faceName
                awhen faceMatMay (objSetMaterial . ma_var)
                mapM_ (flip grpLink objVar) grps

                when (scene_setLongLabelCustomProperties ?scene)
                    (objVar <.> longLabelPropertyName .= str faceName)

-- toBlender :: forall a. (BlenderLabel s, 
--                         VertexInfoClass (Vertex s),
--                         Show (Vertex s), 
--                         Show (Edge s), 
--                         Show (newFlatTriangleObj s), 
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
                methodCall1 (bpy_ops "surface") "primitive_nurbs_surface_sphere_add" 
                    (namedArg "location" (py "loc"))
                returnContextStmt
}


makeCylinderFunDef :: PythonFunDef
makeCylinderFunDef = PythonFunDef {
    pfd_name = "makeCylinder",
    pfd_argList = [],
    pfd_defBody = do
            methodCall (bpy_ops "surface") "primitive_nurbs_surface_cylinder_add" ()
            returnContextStmt

}

vertexGrp ::  BlenderGroup
vertexGrp = BlenderGroup "Vertices"
edgeGrp ::  BlenderGroup
edgeGrp = BlenderGroup "Edges"
triGrp ::  BlenderGroup
triGrp = BlenderGroup "Triangles"
triLblGrp ::  BlenderGroup
triLblGrp = BlenderGroup "TriangleLabels"

extraGrps :: [BlenderGroup]
extraGrps = [
              vertexGrp 
            , edgeGrp 
            , triGrp 
            , triLblGrp 
            ]

toBlender :: (CONSTRAINTS0(s)) => Scene s -> Python ()
toBlender scene@Scene{
        scene_worldProps,
        scene_blenderable = ba@Blenderable{..},
        scene_cams} = do

            ln "from time import time"
            ln "starttime = time()"
            result
            ln "print('Script took ',time()-starttime, ' secs')"

    where
        result = let ?scene = scene in do
            ln "from bpy import *"
            ln "from pprint import pprint"

            mapM_ pfd_def [makeSphereFunDef,makeCylinderFunDef]


            -- Make a new scene
            sceneVar .= methodCallExpr1 (bpy_dat "scenes") "new" (str "TheScene")
            setRenderSettings sceneVar (scene_render scene)
            -- Remove all other scenes
            ln  "for s in data.scenes:"
            indent $ do
                py "if (s != "
                sceneVar
                py "):"
                indent $ do
                    ln  "data.scenes.remove(s)" 

            -- Make a new world, assign it to the scene, set world properties
            worldVar .= methodCallExpr1 (bpy_dat "worlds") "new" (str "TheWorld")
            mapM_ (setProp' worldVar) scene_worldProps
            setProp sceneVar "world" worldVar

            -- cameras
            forM_ (zip [0::Integer ..] scene_cams) (\(i, (Cam pos eulers fov)) -> do
                let nam = "cam"++show i
                camVar .= methodCallExpr1 (bpy_dat "cameras") "new" (str nam)
                camVar <.> "angle" .= fov
                newObj objVar nam camVar
                objVar <.> "location" .= pos
                objVar <.> "rotation_euler" .= eulers
                when (i==0) (setProp sceneVar "camera" objVar))

            -- light
            lightVar .= methodCallExpr (bpy_dat "lamps") "new" (str "TheLamp", str "SUN") 
            lightVar <.> "shadow_method" .= str "RAY_SHADOW"
            newObj objVar "TheLamp" lightVar
            objVar <.> "rotation_euler" .= Vec3 (5*pi/12) 0 (-pi/6)
            objVar <.> "location" .= Vec3 (-5) (-10) 8

            idDefs

            (bpy_types "Object" <.> longLabelPropertyName) .= 
                methodCallExpr1 bpy_props "StringProperty" (str "Simplex label")



            let ds = ba_ds ba
                tcec = mkTCEC ds
                ecvc = mkECVC ds

            mapM_ (handleSimplex dhV ba tcec ecvc) (vertexList ds)
            mapM_ (handleSimplex dhE ba tcec ecvc) (edgeList ds)
            mapM_ (handleSimplex dhT ba tcec ecvc) (triangleList ds)

            --ln "ops.view3d.viewnumpad(type='CAMERA')"

            when (isJust . scene_initialSelection $ scene) $ do
                deselectAllStmt
                toBeSelectedVar <.> "select" .= True


        

        idDefs = do
                -- order matters here!
            mapM_ id_initGlobalVar (id_collectUniqueThings ba :: [BlenderImage])
            mapM_ id_initGlobalVar (id_collectUniqueThings ba :: [Texture])
            mapM_ id_initGlobalVar (id_collectUniqueThings ba :: [Material]) 
            mapM_ id_initGlobalVar (extraGrps ++ id_collectUniqueThings ba :: [BlenderGroup]) 



data SemiProcessedSimplex v e t a = SemiProcessedSimplex {
    sps_mkAsi :: a -> AnySimplex2 v e t,
    sps_group :: BlenderGroup,
    sps_objVarSettingStmts :: Python (),
    sps_trailingStmts :: Python ()
}

finishSimplexProcessing :: (CONSTRAINTS(s)) => SemiProcessedSimplex (Vert s) (Ed s) (Tri s) a -> Blenderable s -> a -> Python ()
finishSimplexProcessing SemiProcessedSimplex{..} ba si =
  case ba_visibility ba asi of

    Invisible -> return ()
    Visible -> do
            sps_objVarSettingStmts
            objCommon 
                (sps_group:bfi_groups) 
                (unFaceName (ba_faceName ba asi)) 
                (Just faceMat)
            when (Just asi == scene_initialSelection ?scene)
                (toBeSelectedVar .= objVar)
            sps_trailingStmts
        

  where
      asi = sps_mkAsi si
      BaFaceInfo {faceMat,bfi_groups} = ba_faceInfo ba asi 

-- | For factoring out the logic specific to a dimension
type DimensionHandler s a =
        (CONSTRAINTS(s)) => 

               Blenderable s
            -> TrianglesContainingEdge_Cache (Ed s) (Tri s) 
            -> EdgesContainingVertex_Cache (Vert s) (Ed s) 
            -> a 
            -> SemiProcessedSimplex (Vert s) (Ed s) (Tri s) a 

handleSimplex
  :: (CONSTRAINTS(s)) =>

        DimensionHandler s a
     -> Blenderable s
     -> TrianglesContainingEdge_Cache (Ed s) (Tri s)
     -> EdgesContainingVertex_Cache (Vert s) (Ed s)
     -> a
     -> Python ()
handleSimplex dh ba tcec ecvc si = 
    finishSimplexProcessing (dh ba tcec ecvc si) ba si 

-- | Logic specific for vertices
dhV :: DimensionHandler s (Vert s)
dhV   ba tcec ecvc v = 
            SemiProcessedSimplex 
                vertToAnySimplex2 
                vertexGrp 
                (newSphereObj objVar (ba_coords ba tcec ecvc v) (ba_vertexThickness ba v)) 
                mempty

-- | Logic specific for edges
dhE :: DimensionHandler s (Ed s)
dhE    ba tcec _ e =
            SemiProcessedSimplex
                edToAnySimplex2
                edgeGrp 
                mkObjDataAndObj
                mempty

            where
                thickness = ba_edgeThickness ba e

                ee = ba_edgeEmbedding ba tcec e

                name = unFaceName . ba_faceName ba . edToAnySimplex2 $ e

                bfi = ba_faceInfo ba . edToAnySimplex2 $ e

                mkObjDataAndObj = do 
                    awhen (getL ba_edgeDecoL ba e) 
                        (\d -> mkEdgeDecos ee d name (bfi_labelMat bfi))

                    case ee of 

                        FlatEdge cv0 cv1 -> newCylinderObj objVar cv0 cv1 thickness

                        -- curved triangle
                        GeneralEdge gee -> 
                            
                                newCurveObj objVar (BlenderCurve {
                                    curve_base = BlenderCurveBase {
                                        curve_name = "SomeCurvedEdge",
                                        curve_mats = [], -- set in finishSimplexProcessing 
                                        curve_resolution_u = 4
                                    },
                                    bevel_depth = thickness,
                                    bevel_resolution = 10,
                                    curve_splines = [(nurbsFromFun 2) gee]
                                })
                            

                            
mkEdgeDecos :: EdgeEmbedding -> EdgeDeco -> String -> Maybe Material -> Python ()
mkEdgeDecos ee (EdgeDeco n dir) edgeName mat = do
    forM_ (case n of
                1 -> [0.5]
                _ -> equidistantPoints Closed Closed 
                        (0.5-decoStretchFraction/2) (0.5+decoStretchFraction/2) (n-1))

          $ \(u_center :: Double) ->

        let
            u_min = u_center - decoConeLengthFraction/2
            u_max = u_center + decoConeLengthFraction/2

        in
            coneAlongCurve 
                ((\(t :: AD s Double) -> 
                    let
                        t_ = interpolU t (lift u_min) (lift u_max)
                    in
                        $(assrt [| not (isNaN t_ || isInfinite t_) |] ['n,'u_center,'u_min,'u_max,'t,'t_]) $
                            evalEdgeEmbedding ee t_) 

                    . case dir of
                           NoFlip -> id
                           Flip -> (\x -> 1-x))


                decoConeWidth 
                ("DecoCone"++show n++"Of "++edgeName) 
                mat



coneAlongCurve :: UF Tup3 Double -> Double -> String -> Maybe Material -> Python ()
coneAlongCurve (c :: UF Tup3 Double) width name mat = 

    $(assrt [| isKindaUnitLength tangent && isKindaUnitLength other0 && isKindaUnitLength other1 |] ['tangent,'other0,'other1]) $ 
    $(assrt [| areKindaOrthogonal other0 other1 |] ['tangent,'other0,'other1]) $ 
    $(assrt [| areKindaOrthogonal tangent other0 |] ['tangent,'other0,'other1]) $ 
    $(assrt [| areKindaOrthogonal tangent other1 |] ['tangent,'other0,'other1]) $ 

--     trace 
--         (docToString $ 
--             prettyEqs [("dist", pretty $ norm (baseVertex 0 &- baseVertex (div n 2)))
--                       ,("tangent",pretty $ tangent)
--                       ,("other0",pretty $ other0)
--                       ,("other1",pretty $ other1)
--                       ]) $

        do
            newMeshObj (py "decoCone") 
                Mesh {
                    meshName = name,
                    meshSmooth = True,
                    meshMats = maybeToList mat,
                    uv_textures = [],
                    meshVertices = apex : map baseVertex [0..n-1],
                    meshFaces = [ (0,i,j)
                                    
                                  | let is = n:[1..n], 
                                    (i,j) <- zip is (drop 1 is) ]
                }






  where
        tangent,other0,other1,valueAt0 :: Tup3 Double
        tangent = twoNormalize (diffF c 0)
        other0 = twoNormalize (anyOrthT3 tangent) -- low-priority fixme: will lead to ugly results if we move over a discontinuity of anyOrthT3 as 'u' varies 
        other1 = cross3 tangent other0  

        valueAt0 = lowerUF c 0
        apex = tup3toVec3 $ lowerUF c 1

        n = 20

        baseVertex i = tup3toVec3 $
            let
                long = 2 * pi * fi i / fi n

                d = 
                        
                            (other0 ^* cos long) 
                        ^+^
                            (other1 ^* sin long)
            in

                
                (valueAt0 ^+^
                    (d ^* width))





                        

-- | Logic specific for triangles
dhT :: DimensionHandler s (Tri s)
dhT  ba tcec ecvc t = 
            SemiProcessedSimplex
                triToAnySimplex2
                triGrp
                stmts1
                stmts2

            where
                stmts1 = do
                    case ba_triangleEmbedding ba t of
                        FlatTriangle cv0 cv1 cv2 -> 
                            newFlatTriangleObj objVar cv0 cv1 cv2 name
                        GeneralTriangle (GTE res g) -> do 
                            newTriangularSurfaceObj res objVar g name
                            mkHelpLines g helpLineMat 


                    objVar <.> "show_transparent" .= True

                stmts2 = handleTriLabel ba tcec ecvc triLblGrp t


                bfi = getL ba_triangleInfoL ba $ t
                helpLineMat = bfi_helpLineMat bfi 
                name = unFaceName $ ba_faceName ba (triToAnySimplex2 t)


        
handleTriLabel :: (CONSTRAINTS(s)) => 

        Blenderable s 
    ->  TrianglesContainingEdge_Cache (Ed s) (Tri s) 
    ->  EdgesContainingVertex_Cache (Vert s) (Ed s) 
    ->  BlenderGroup -> Tri s -> Python ()

handleTriLabel ba tcec ecvc grpTriLabels t =
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

                        m = --   $(traceExps "tri" ['t,'text,'textCenterBase,'thescale]) $ 

                            scalingUniformProj4 thescale 
                            .*.
                            safeOrthogonal (Mat3 rightunit upunit (crossprod rightunit upunit)) 
                            .*.
                            translation textCenterBase


                        groups = grpTriLabels
                                    : (bfi_groups . ba_faceInfo ba . triToAnySimplex2 $ t)


                        mat = bfi_labelMat . getL ba_triangleInfoL ba $ t


                    in do
                        newTextObj objVar
                            (TextCurve {
                                textCurve_base = BlenderCurveBase {
                                    curve_name = "SomeTextCurve",
                                    curve_mats = maybeToList mat,
                                    curve_resolution_u = 12
                                },
                                textCurve_text = text,
                                textCurve_extrude = textThickness
                            })
                        objVar <.> matrix_basis .= m
                        objCommon 
                            groups 
                            (text ++ " on "++show perm++" "++unFaceName (ba_faceName ba asi))
                            Nothing


    where
        vs = vertices t
        cvs = map3 (ba_coords ba tcec ecvc) vs

        asi = triToAnySimplex2 t


        
newSphereObj :: PythonVar -> Vec3 -> Double -> Python ()
newSphereObj var loc radius = do
            var .= pfd_call1 makeSphereFunDef loc
            var <.> "scale" .= (Vec3 radius radius radius)




newCylinderObj :: PythonVar -> Vec3 -> Vec3 -> Double -> Python ()
newCylinderObj var from to radius = 
    if normsqr (from &- to) <= 1E-14
       then newEmptyObj var "Crushed newCylinderObj"
       else 

    
        do
            var .= pfd_call makeCylinderFunDef ()

            var <.> matrix_basis .= m

            where
                m :: Proj4
                m = 
                    
                    scaling (Vec3 radius radius (1/2)) .*. 
                    translation ((1/2) *& vec3Z) .*.
                    linear (pointZTo (to &- from)) .*.
                    translation from






        
matrix_basis ::  [Char]
matrix_basis = "matrix_basis"

newFlatTriangleObj
  :: PythonVar -> MeshVertex -> MeshVertex -> MeshVertex -> String -> Python ()
newFlatTriangleObj var p0 p1 p2 name =
    newMeshObj var (Mesh name [p0,p1,p2] [(0,1,2)] False [] [])


safeOrthogonal :: Mat3 -> Proj4
safeOrthogonal m = 
    assert (matrixApproxEq (m .*. transpose m) idmtx) $
    assert (matrixApproxEq (transpose m .*. m) idmtx) $
    linear m




testBlender :: (CONSTRAINTS0(s)) => Scene s -> IO ExitCode
testBlender s = do
    let fn = "/tmp/testBlender.py"
    starttime <- getCurrentTime
    {-# SCC "testBlender/writeFile" #-} renderPythonToFile fn (toBlender s)
    endtime <- getCurrentTime
    putStrLn ("toBlender took "++show (endtime `diffUTCTime` starttime)) 
    putStrLn ("Scene creation script written to " ++ fn++"; launching Blender")
    args <- getArgs
    rawSystem "blender" ("-P":fn:args)


meshVar ::  Python ()
meshVar = py "me"



newMeshObj :: PythonVar -> Mesh -> Python ()
newMeshObj var m =
    do 
        let _meshVar = py "decoConeMesh"

        mesh_init _meshVar m
        newObj var (meshName m) _meshVar 
        
--         setActiveObject sceneVar var
--         -- don't know how to determine whether we are in editmode already (seems to be determined at random for new objects...), so try/catch
--         ln "try:"
--         indent $ do
--             normals_make_consistent
--         ln "except RuntimeError:" 
--         indent $ do
--             editmode_toggle
--             normals_make_consistent

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

txtVar ::  Python ()
txtVar = py "txt"

textThickness :: Double
textThickness = 1E-5

newTextObj :: PythonVar -> TextCurve -> Python ()
newTextObj var txt = do
    textCurve_init txtVar txt 
    newObj var (id_name txt) txtVar



newCurveObj :: PythonVar -> BlenderCurve -> Python ()
newCurveObj var (cu :: BlenderCurve) = do
    curve_init curveVar cu
    newObj var (id_name cu) curveVar

    

-- | Creates a new object with the given name and data, links it to the scene, assigns it to the given var

newObj :: ToPython t => PythonVar -> String -> t -> Python ()
newObj (var :: PythonVar) name objData = do
            var .= (methodCallExpr (bpy_dat "objects") "new" (str name,objData))
            methodCall1 (sceneVar <.> "objects") "link" var

newEmptyObj :: PythonVar -> String -> Python ()
newEmptyObj var name = newObj var name none

--linkObjToScene =

grpLink :: BlenderGroup -> Python () -> Python ()
grpLink gr obj = methodCall1 (bgrp_objectsE gr) "link" obj


-- note: we usually set spline_resolution_u low (1 or 2) and instead compute subdivisions here by increasing 'steps'
nurbsFromFun :: Int -> GeneralEdgeEmbedding -> BlenderSpline
nurbsFromFun spline_resolution (GEE steps f) =
    Nurbs {
        spline_dimOpts = SplineDimOpts {
            use_endpoint = True,
            order = 3,
            spline_resolution,
            use_cyclic = normsqr (fv 0 - fv 1) < 1E-10
        },
        spline_points = ys
    }

    where
            m = steps - 1

            fv = tup3toVec3 . lowerUF f

            ys = V.generate steps (\i -> fv (fi i / fi m))




        --sequence_ <$> zipWithM (\x y -> newCylinderObj x y thickness) ys (tail ys)
            

newTriangularSurfaceObj :: 
        Int 
    -> PythonVar
    -> FF Tup2 Tup3 Double
    -> String
    -> Python ()
newTriangularSurfaceObj = memo prepare
    where
        prepare (steps :: Int) = 
            let
                m = steps - 1
                vertexNumbering = nuFromDistinctList vertices_intUV 
                toi = toInt vertexNumbering

                (vertices_intUV,tris_intUV) = 
                    triangulateTriangle steps
                
                
                        
                tris = map (map3 toi) tris_intUV

                normalizeIntUV (u,v) = Vec2 (fi u / fi_m) (fi v / fi_m)

                vertices_normalizedUV = map normalizeIntUV vertices_intUV
                tris_normalizedUV = map (map3 normalizeIntUV) tris_intUV

                fi_m = fi m
            in
                \var (emb :: FF Tup2 Tup3 Double) name -> do
                    newMeshObj var
                        (Mesh {
                            meshName = name,
                            meshVertices = map 
                                            (tup3toVec3 . lowerFF emb . vec2toTup2)
                                            vertices_normalizedUV,

                            meshFaces = tris,
                            meshSmooth = True,
                            uv_textures = 
                                [ MeshUVLayer triangleUVLayerName tris_normalizedUV ] ,

                            meshMats = []

                          })




mkHelpLines :: FF Tup2 Tup3 Double -> Maybe Material -> Python ()
mkHelpLines (emb :: FF Tup2 Tup3 Double) helpLineMat = do
    newCurveObj (py "helpLine") (BlenderCurve {
        curve_base = BlenderCurveBase {
            curve_name = "HelpLineCurve",
            curve_resolution_u = 1,
            curve_mats = maybeToList helpLineMat
        },
        bevel_depth = helpLineThickness,
        bevel_resolution = 4,
        curve_splines = map (nurbsFromFun 1) 
            ( concat 
                [ 
                [ GEE steps' (emb . (\x -> tup2 (x*lift p') (lift p))) -- horiz 
                , GEE steps' (emb . (\x -> tup2 (lift p) (x*lift p'))) -- vert
                , GEE steps' (emb . (\x -> lift p' *^ interpol x tup2X tup2Y)) -- diag
                ]

                    |  i <- [ 1 .. helpLineN ]

                            -- runs from 1/(n+1) to n/(n+1)
                    ,  let p = fi i/fi (helpLineN + 1) 
                            -- runs from n/(n+1) to 1/(n+1)
                    ,  let p' = 1-p  
--                                     ,  let steps' = (max 4 (round (fi helpLineMaxSteps * p'))) :: Int
                    ,  let steps' = helpLineMaxSteps :: Int
                        
                        
                ] 
                )
        })





-- viewSelectedStmt :: Python ()
-- viewSelectedStmt = methodCall (bpy_ops "view3d") "view_selected" ()

deselectAllStmt :: Python ()
deselectAllStmt = foreachStmt "o" (py "context.selected_objects") 
    (\o -> o <.> "select" .= False)



setRenderSettings :: ToPython a => a -> RenderSettings -> Python ()
setRenderSettings _sceneVar RS{..} = do
    _sceneVar <.> "render.resolution_x" .= render_resolution_x
    _sceneVar <.> "render.resolution_y" .= render_resolution_y
    awhen render_filepath (\fp -> _sceneVar <.> "render.filepath" .= str fp)

{-# LANGUAGE RankNTypes, ImplicitParams, GADTs, NamedFieldPuns, FlexibleContexts, TemplateHaskell, ScopedTypeVariables, PolymorphicComponents, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, CPP, GeneralizedNewtypeDeriving, TypeFamilies, DefaultSignatures, ExtendedDefaultRules, StandaloneDeriving #-} 
{-# OPTIONS -Wall -fwarn-missing-local-sigs -fno-warn-unused-imports -fno-warn-missing-signatures #-}
module Blender(
    module Blender.Types,
    module Blender.ID,
    module Blender.Conversion,
    module Blender.Blenderable,
    module Blender.Mesh,
    module MathUtil,
    testBlender,
    toBlender,
    renderBlender,
    blenderMain,
    BCommand(..)) where

import PreRenderable
import Simplicial.DeltaSet2
import Blender.Blenderable
import Blender.Mesh
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
import Data.Vect.Double(Vec3(..),norm,scalingUniformProj4,normalize,dotprod,Mat3(..),crossprod,translation,normsqr)
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
import PrettyUtil(pretty,text,prettyString)
import Data.AdditiveGroup((^-^))
import Blender.Build
import Text.Groom
import Data.Proxy

#define CONSTRAINTS0(s) Show (Vert s), Show (Ed s), Show (Tri s), PreDeltaSet2 s, Ord (Ed s), Ord (Vert s), Ord (Tri s)
#define CONSTRAINTS(s) ?sceneE::Python(),?scene::Scene s, CONSTRAINTS0(s) 





-- * Edge-deco-related constants

-- Absolute units
decoConeLength :: BlenderUnits
decoConeLength = 0.12


cylLength :: BlenderUnits
cylLength = 0.15*decoConeLength

gapfactor = 2

conegap :: BlenderUnits
conegap = 0.07 * gapfactor

-- | Gap between the last cone and the first cyl
ccgap :: BlenderUnits
ccgap = 0.08 * gapfactor

cylgap :: BlenderUnits
cylgap = 0.025 * gapfactor


-- | Deco cone width at the base, as a multiple of edge width
decoConeWidthFactor :: Double
decoConeWidthFactor = 2.5 * (decoConeLength/0.07)
                
-- End of Edge-deco-related constants







toBeSelectedVar :: Python ()
toBeSelectedVar = py "toBeSelected"


ma_var ::  Material -> Python ()
ma_var = blenderGlobalVarFor




longLabelPropertyName :: [Char]
longLabelPropertyName = "longlabel"

objCommon :: (?scene::Scene s) => PythonVar -> [BlenderGroup] -> String -> Maybe Material -> Python ()
objCommon _objVar grps faceName faceMatMay = do
                setName faceName _objVar
                awhen faceMatMay (flip setMaterial _objVar . ma_var)
                mapM_ (flip grpLink _objVar) grps

                when (scene_setLongLabelCustomProperties ?scene)
                    (_objVar <.> longLabelPropertyName .= str faceName)

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


data BCommand = DoRender | JustLook
    deriving (Show,Eq)

-- toBlender :: (CONSTRAINTS0(s)) => Scene s -> BCommand -> Python ()
toBlender scene@Scene{
        scene_world,
        scene_blenderable = ba@Blenderable{..},
        scene_cams} bcmd = do

            ln "from time import time"
            ln "starttime = time()"
            result
            ln "print('Script took ',time()-starttime, ' secs')"

    where
        result = let ?scene = scene in do
            ln "from bpy import *"
            ln "from pprint import pprint"

            -- Make a new scene
            let ?sceneE = pyv "scene"
            ?sceneE .= methodCallExpr1 (bpy_dat "scenes") "new" (str "TheScene")

            assignRenderSettingsProperties ?sceneE (scene_render scene)
            -- Remove all other scenes
            ln  "for s in data.scenes:"
            indent $ do
                py "if (s != "
                ?sceneE
                py "):"
                indent $ do
                    ln  "data.scenes.remove(s)" 

            -- Make a new world, assign it to the scene, set world properties
            let worldVar = pyv "world" in do
                blenderInit worldVar scene_world
                ?sceneE <.> "world" .= worldVar

            -- cameras
            forM_ (zip [0::Integer ..] scene_cams) (\(i, (Cam pos eulers fov)) -> do
                let objVar = pyv "camObj"
                newCamObj objVar 
                    (BObj 
                        ("CamObj"++show i) 
                        (BCamD { bcamd_name = "Cam"++show i, bcamd_FOV = fov }) 
                        (LocAndEulers pos eulers))

                when (i==0) (?sceneE <.> "camera" .= objVar))

            let objVar = pyv "lightObj" in 
                forM_ (scene_lamps scene) (newLampObj objVar) 

            idDefs

            (bpy_types "Object" <.> longLabelPropertyName) .= 
                methodCallExpr1 bpy_props "StringProperty" (str "Simplex label")



            let ds = ba_ds ba

            mapM_ (handleSimplex dhV ba) (vertexList ds)
            mapM_ (handleSimplex dhE ba) (edgeList ds)
            mapM_ (handleSimplex dhT ba) (triangleList ds)

            --ln "ops.view3d.viewnumpad(type='CAMERA')"

            when (isJust . scene_initialSelection $ scene) $ do
                deselectAllStmt
                toBeSelectedVar <.> "select" .= True


            case bcmd of
                 JustLook -> return ()
                 DoRender -> do
                     ?sceneE <.> "render.resolution_percentage" .= (100::Int)
                     methodCall (bpy_ops "render") "render" (namedArg1 "write_still" True)
                     quit_blender


        

        idDefs = do
                -- order matters here!
            blenderInitGlobalVars scene (undefined :: Proxy BlenderImage)
            blenderInitGlobalVars scene (undefined :: Proxy Texture)
            blenderInitGlobalVars scene (undefined :: Proxy Material)
            blenderInitGlobalVars scene (undefined :: Proxy BlenderGroup)
            mapM_ blenderInitGlobalVar extraGrps




asiCommon
  :: (CONSTRAINTS(s)) => Blenderable s -> AnySimplex2Of s -> BlenderGroup -> PythonVar -> Python ()
asiCommon ba asi group objE = do
            objCommon objE 
                (group:bfi_groups) 
                (unFaceName (ba_faceName ba asi)) 
                (Just faceMat)
            when (Just asi == scene_initialSelection ?scene)
                (toBeSelectedVar .= objE)
    where
        BaFaceInfo {faceMat,bfi_groups} = ba_faceInfo ba asi 

-- | For factoring out the logic specific to a dimension
type DimensionHandler s a =
        (CONSTRAINTS(s)) => 

               Blenderable s
            -> a 
            -> Python ()

handleSimplex
  :: (CONSTRAINTS(s)) =>

        DimensionHandler s a
     -> Blenderable s
     -> a
     -> Python ()
handleSimplex dh ba si = do
    ln ""
    dh ba si

-- | Logic specific for vertices
dhV :: DimensionHandler s (Vert s)
dhV   ba v = 
    when (ba_visibility ba v' == Visible) $ do
            let _objVar = pyv "sphereObj"
            (newSphereObj _objVar (ba_coords ba v) (ba_vertexThickness ba v)) 
            asiCommon ba v' vertexGrp _objVar

  where
    v' = vertToAnySimplex2 v

-- | Logic specific for edges
dhE :: DimensionHandler s (Ed s)
dhE    ba e =
    when (ba_visibility ba e' == Visible) $ do 
        awhen (getL ba_edgeDecoL ba e) 
            (\d -> mkEdgeDecos ee d name (bfi_labelMat bfi) (bfi_labelMat2 bfi) thickness)

        let _objVar = pyv "cylObj"

        case ee of 

            FlatEdge cv0 cv1 -> newCylinderObj _objVar cv0 cv1 thickness

            -- curved triangle
            GeneralEdge gee -> 
                
                    newCurveObj _objVar (BlenderCurve {
                        curve_base = BlenderCurveBase {
                            curve_name = "SomeCurvedEdge",
                            curve_mats = [], -- set in finishSimplexProcessing 
                            curve_resolution_u = 4
                        },
                        bevel_depth = thickness,
                        bevel_resolution = 10,
                        curve_splines = [(nurbsFromFun 2) gee]
                    })

        asiCommon ba e' edgeGrp _objVar

            where
                e' = edToAnySimplex2 e
                
                thickness = ba_edgeThickness ba e

                ee = ba_edgeImmersion ba e

                name = unFaceName . ba_faceName ba $ e' 

                bfi = ba_faceInfo ba $ e'

                            

                            
mkEdgeDecos :: (?scene::Scene s,?sceneE::Python ()) => EdgeImmersion -> EdgeDeco -> String -> Maybe Material -> Maybe Material -> BlenderUnits -> Python ()
mkEdgeDecos ee (EdgeDeco n dir) edgeName coneMat cylMat edgeThickness = do

    let 
        width = decoConeWidthFactor * edgeThickness 

        ee' :: UF Tup3 Double
        ee' = evalEdgeImmersion ee . (case dir of
                                        NoFlip -> id
                                        Flip -> (\x -> 1-x))

    
        -- in absolute distance on the edge, from the midpoint of the edge
        (cone_positions, cyl_positions) = 
            let
                
                (ncyls,ncones) = 
                    let (d,m) = divMod n 3 in if m==0 then (d-1,3) else (d,m)

                _len = conegap * fi (ncones-1) 
                      + if ncyls == 0 then 0 else (ccgap + cylgap * fi (ncyls-1)) 

                cyl_u i = - _len/2 + fi i * cylgap
                cone_u i =
                    if ncyls == 0 
                       then - _len/2 + fi i * conegap 
                       else cyl_u (ncyls - 1) + ccgap + fi i * conegap

            in
                ( map cone_u [0..ncones-1]
                , map cyl_u [0..ncyls-1] )

        speed_midpoint = twoNorm (diffF ee' 0.5)
        -- linear approx

        absPosToU x = 0.5 + x/speed_midpoint 

        cone_us = map absPosToU cone_positions 
        cyl_us  = map absPosToU cyl_positions

    forM_ cone_us $ \(u_center :: Double) ->
        let
            speed_u_center = twoNorm (diffF ee' u_center)  
            u_min = u_center - (decoConeLength/(2*speed_u_center))
            u_max = u_center + (decoConeLength/(2*speed_u_center))
        in
            coneAlongCurve 
                ((\(t :: AD s Double) -> 
                        ee' (interpolU t (lift u_min) (lift u_max))))

                width
                ("DC"++edgeName) 
                coneMat



    forM_ cyl_us $ \u_center -> 

        let
            value = lowerUF ee' u_center
            unitTangent = twoNormalize (diffF ee' u_center)  
            var = py "decoCyl"
        in do
            newCylinderMeshObj var 
                (tup3toVec3 (value ^-^ unitTangent ^* (cylLength/2)))
                (tup3toVec3 (value ^+^ unitTangent ^* (cylLength/2)))
                width
                128
                False

            objCommon var [] ("DecoCylOf "++edgeName) cylMat



coneAlongCurve :: (?sceneE::Python()) => UF Tup3 Double -> Double -> String -> Maybe Material -> Python ()
coneAlongCurve (c :: UF Tup3 Double) width name mat = 

    $(assrt [| isKindaUnitLength tangent && isKindaUnitLength other0 && isKindaUnitLength other1 |] ['tangent,'other0,'other1]) $ 
    $(assrt [| areKindaOrthogonal other0 other1  |] ['tangent,'other0,'other1]) $ 
    $(assrt [| areKindaOrthogonal tangent other0 |] ['tangent,'other0,'other1]) $ 
    $(assrt [| areKindaOrthogonal tangent other1 |] ['tangent,'other0,'other1]) $ 


--     trace ("meshName = " ++ name) $
--     trace ("meshVertices =\n" ++ prettyString meshVertices) $

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
                    meshVertices,
                    meshFaces
                }






  where
        meshVertices = apex : map baseVertex [0 .. n-1]

        meshFaces :: [Triple MeshVertexIndex]
        meshFaces = [ (0,i,j)
                        
                        | let is = n:[1..n] :: [MeshVertexIndex], 
                        (i,j) <- zip is (drop 1 is) ]

        tangent,other0,other1,valueAt0 :: Tup3 Double
        tangent = twoNormalize (diffF c 0)
        other0 = twoNormalize (anyOrthT3 tangent) -- low-priority fixme: will lead to ugly results if we move over a discontinuity of anyOrthT3 as 'u' varies 
        other1 = cross3 tangent other0  

        valueAt0 = lowerUF c 0
        apex = tup3toVec3 $ lowerUF c 1

        n = 20 :: Int

        baseVertex :: Int -> MeshVertex
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
dhT  ba t = do 
    when (vis==Visible) $ do
        stmts1
        asiCommon ba t' triGrp _objVar
    when (vis/=Invisible) $ do
        handleTriLabel ba triLblGrp t

            where
                _objVar = pyv "triObj"

                t' = triToAnySimplex2 t

                vis = ba_visibility ba t'

                stmts1 = do
                    case ba_triangleImmersion ba t of
                        FlatTriangle cv0 cv1 cv2 -> 
                            newFlatTriangleObj _objVar cv0 cv1 cv2 name
                        GeneralTriangle (GTE _ res g) -> do 
                            newTriangularSurfaceObj res _objVar g name

                            awhen (bfi_helpLineSettings bfi) (mkHelpLines g)


                    _objVar <.> "show_transparent" .= True



                bfi = getL ba_triangleInfoL ba $ t
                name = unFaceName $ ba_faceName ba t'


        
handleTriLabel :: (CONSTRAINTS(s)) => 

        Blenderable s 
    ->  BlenderGroup -> Tri s -> Python ()

handleTriLabel ba grpTriLabels t =
            case ba_triangleLabel ba t of
                Nothing -> return ()

                Just tl -> 
                    let
                        _text = tl_text tl
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
--                                 -- correct sign error parity
--                             safeOrthogonal (Mat3 vec3X vec3Y (neg vec3Z))
--                             .*.
                            safeOrthogonal 
                                (Mat3 
                                    rightunit 
                                    upunit 
                                    (crossprod rightunit upunit))
                            .*.
                            translation textCenterBase


                        groups = grpTriLabels
                                    : (bfi_groups . ba_faceInfo ba . triToAnySimplex2 $ t)


                        mat = bfi_labelMat . getL ba_triangleInfoL ba $ t


                    in do
                        let objVar = pyv "textObj"
                        newTextObj objVar
                            (TextCurve {
                                textCurve_base = BlenderCurveBase {
                                    curve_name = "SomeTextCurve",
                                    curve_mats = maybeToList mat,
                                    curve_resolution_u = 12
                                },
                                textCurve_text = _text,
                                textCurve_extrude = textThickness
                            })
                        objVar <.> matrix_basis .= m
                        objCommon objVar
                            groups 
                            (_text ++ " on "++show perm++" "++unFaceName (ba_faceName ba asi))
                            Nothing


    where
        vs = vertices t
        cvs = map3 (ba_coords ba) vs

        asi = triToAnySimplex2 t


        




-- testBlender :: (CONSTRAINTS0(s)) => Scene s -> IO ExitCode
testBlender = blenderMain JustLook

renderBlender = blenderMain DoRender



blenderMain doRender s = do
    when (doRender==DoRender) (putStrLn "BATCH RENDERING MODE")
    let fn = "/tmp/testBlender.py"
    starttime <- getCurrentTime

    {-# SCC "testBlender/writeFile" #-} renderPythonToFile fn (toBlender s doRender)
    endtime <- getCurrentTime
    putStrLn ("toBlender took "++show (endtime `diffUTCTime` starttime)) 
    putStrLn ("Scene creation script written to " ++ fn++"; launching Blender")
    -- args <- getArgs
    rawSystem "blender" ("-P":fn:[]) -- :fn:args)


meshVar ::  Python ()
meshVar = do 

        py "me"
        


txtVar ::  Python ()
txtVar = py "txt"

textThickness :: Double
textThickness = 1E-5



-- note: we usually set spline_resolution_u low (1 or 2) and instead compute subdivisions here by increasing 'steps'
nurbsFromFun :: Int -> GeneralEdgeImmersion -> BlenderSpline
nurbsFromFun spline_resolution (GEE _ steps f) =
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
            

newTriangularSurfaceObj :: (?sceneE::Python ()) => 
        Int 
    -> PythonVar
    -> FF Tup2 Tup3 Double
    -> String
    -> Python ()
newTriangularSurfaceObj = memo prepare
    where
        prepare :: (?sceneE::Python ()) =>
                          Int -> PythonVar -> FF Tup2 Tup3 Double -> String -> Python ()
        prepare (steps :: Int) = 
            let
                m = steps - 1
                vertexNumbering = nuFromDistinctUnboxList vertices_intUV 
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




mkHelpLines :: (?sceneE::Python ()) => FF Tup2 Tup3 Double -> HelpLineSettings -> Python ()
mkHelpLines (emb :: FF Tup2 Tup3 Double) HLS{..} = do
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
                [ GEE (text "<helpLines>") steps' 
                    (emb . (\x -> tup2 (x*lift p') (lift p))) -- horiz 
                , GEE (text "<helpLines>") steps' 
                    (emb . (\x -> tup2 (lift p) (x*lift p'))) -- vert
                , GEE (text "<helpLines>") steps' 
                    (emb . (\x -> lift p' *^ interpol x tup2X tup2Y)) -- diag
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


{-# LANGUAGE ImplicitParams, GADTs, NamedFieldPuns, FlexibleContexts, TemplateHaskell, ScopedTypeVariables, PolymorphicComponents, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, CPP, GeneralizedNewtypeDeriving, TypeFamilies, DefaultSignatures, ExtendedDefaultRules, StandaloneDeriving #-} 
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module Blender(
    module Blender.Conversion,
    module Blender.Blenderable,
    module MathUtil,
    testBlender,
    toBlender) where

import Blender.Conversion
import Blender.Blenderable
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Function
import Data.MemoTrie
import Data.Vect.Double hiding((*.),(.*.))
import FaceClasses
import HomogenousTuples
import MathUtil
import Data.Numbering
import PreRenderable
import Prelude hiding(catch,mapM_,sequence_) 
import PrettyUtil(Pretty)
import Simplicial.DeltaSet2
import System.Environment
import System.Exit
import System.Process
import THUtil
import ToPython
import Util
import qualified Data.Vector as V
import Data.Monoid
import Data.Maybe
import qualified Data.Foldable as Fold
import Data.Foldable(Foldable)

#define CONSTRAINTS0(s) Pretty (Vert s), Pretty (Ed s), Pretty (Tri s), DeltaSet2 s, Pretty s, Ord (Ed s), Ord (Vert s), Ord (Tri s)
#define CONSTRAINTS(s) ?scene::Scene s, CONSTRAINTS0(s) 

newtype BlenderGroupVarName = BlenderGroupVarName { bgvn_toString :: String }

instance ToPython BlenderGroupVarName where
    toPython = py . bgvn_toString

toBeSelectedVar,sceneVar,worldVar,camVar,objVar,lightVar,curveVar,splineVar,meshTextureFaceLayerVar :: Python ()

sceneVar = py "scene"
worldVar = py "world"
camVar = py "cam"
objVar = py "obj"
lightVar = py "light"
curveVar = py "curve"
splineVar = py "spline"
meshTextureFaceLayerVar = py "meshTextureFaceLayer"
toBeSelectedVar = py "toBeSelected"


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
triLabelMat = Material "triLbl" [ let r = 0.015 in diffuseColor (r,r,r) ] Nothing

longLabelPropertyName :: [Char]
longLabelPropertyName = "longlabel"

objCommon :: (?scene::Scene s) => [BlenderGroupVarName] -> String -> Material -> Python ()
objCommon grps faceName faceMat = do
                objSetName faceName
                objSetMaterial (ma_var faceMat)
                mapM_ (flip grpLink objVar) grps

                when (scene_setLongLabelCustomProperties ?scene)
                    (objVar <.> longLabelPropertyName .= str faceName)

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

vertexGrpName ::  BlenderGroupVarName
vertexGrpName = BlenderGroupVarName "grp_v"
edgeGrpName ::  BlenderGroupVarName
edgeGrpName = BlenderGroupVarName "grp_e"
triGrpName ::  BlenderGroupVarName
triGrpName = BlenderGroupVarName "grp_t"
triLblGrpName ::  BlenderGroupVarName
triLblGrpName = BlenderGroupVarName "grp_tlbl"
mkGrpVarName ::  BlenderGroupName -> BlenderGroupVarName
mkGrpVarName = BlenderGroupVarName . ("grp__" ++)

toBlender :: (CONSTRAINTS0(s)) => Scene s -> Python ()
toBlender scene@Scene{
        scene_worldProps,
        scene_blenderable = ba@Blenderable{..},
        scene_cams} =

            result

    where
        result = let ?scene = scene in do
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
            newGroup vertexGrpName "Vertices" 
            newGroup edgeGrpName "Edges" 
            newGroup triGrpName "Triangles" 
            newGroup triLblGrpName "Triangle labels" 
            forM_ (ba_uniqueGroups ba) (\x -> newGroup (mkGrpVarName x) x)

            (types "Object" <.> longLabelPropertyName) .= 
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

        


        materialDefs = mapM_ materialToBlender (triLabelMat : ba_uniqueMaterials ba) 



data SemiProcessedSimplex s a = SemiProcessedSimplex {
    sps_mkAsi :: a -> AnySimplex2Of s,
    sps_group :: BlenderGroupVarName,
    sps_objVarSettingStmts :: Python (),
    sps_trailingStmts :: Python ()
}

finishSimplexProcessing :: (CONSTRAINTS(s)) => SemiProcessedSimplex s a -> Blenderable s -> a -> Python ()
finishSimplexProcessing SemiProcessedSimplex{..} ba si =
  case ba_visibility ba asi of

    Invisible -> return ()
    Visible -> do
            sps_objVarSettingStmts
            objCommon 
                (sps_group:map mkGrpVarName bfi_groups) 
                (unFaceName (ba_faceName ba asi)) 
                faceMat
            when (Just asi == scene_initialSelection ?scene)
                (toBeSelectedVar .= objVar)
            sps_trailingStmts
        

  where
      asi = sps_mkAsi si
      BaFaceInfo {faceMat,bfi_groups} = ba_faceInfo ba asi 

-- | For factoring out the logic specific to a dimension
newtype DimensionHandler s a = DimensionHandler { 
    runDimensionHandler ::
        (CONSTRAINTS(s)) => 

               Blenderable s
            -> TrianglesContainingEdge_Cache (Ed s) (Tri s) 
            -> EdgesContainingVertex_Cache (Vert s) (Ed s) 
            -> a 
            -> SemiProcessedSimplex s a 
} 

handleSimplex
  :: (CONSTRAINTS(s)) =>

        DimensionHandler s a
     -> Blenderable s
     -> TrianglesContainingEdge_Cache (Ed s) (Tri s)
     -> EdgesContainingVertex_Cache (Vert s) (Ed s)
     -> a
     -> Python ()
handleSimplex dh ba tcec ecvc si = 
    finishSimplexProcessing (runDimensionHandler dh ba tcec ecvc si) ba si 

-- | Logic specific for vertices
dhV :: (CONSTRAINTS(s)) => DimensionHandler s (Vert s)
dhV = DimensionHandler f
    where
        f ba tcec ecvc v = 
            SemiProcessedSimplex 
                vertToAnySimplex2 
                vertexGrpName 
                (sphere (ba_coords ba tcec ecvc v) (ba_vertexThickness ba v)) 
                mempty

-- | Logic specific for edges
dhE :: (CONSTRAINTS(s)) => DimensionHandler s (Ed s)
dhE = DimensionHandler f
    where
        f ba tcec grp e =
            SemiProcessedSimplex
                edToAnySimplex2
                edgeGrpName 
                (either handleErr id $ mkObjDataAndObj)
                mempty

            where
                handleErr err = error (err++"\n"++"In handleE\n"++ $(showExps ['ba,'e]))
                thickness = ba_edgeThickness ba e

                mkObjDataAndObj = 
                    case ba_edgeEmbedding ba tcec e of 

                        FlatEdge cv0 cv1 -> cylinder cv0 cv1 thickness

                        -- curved triangle
                        GeneralEdge (GEE res emb) -> 
                            blenderCurvedEdge res thickness emb
                        

-- | Logic specific for triangles
dhT :: (CONSTRAINTS(s)) => DimensionHandler s (Tri s)
dhT = DimensionHandler f
    where
    
        f ba tcec ecvc t = 
            SemiProcessedSimplex
                triToAnySimplex2
                triGrpName
                stmts1
                stmts2

            where
                stmts1 = do
                    case ba_triangleEmbedding ba t of
                        FlatTriangle cv0 cv1 cv2 -> blenderTriangle cv0 cv1 cv2 
                        GeneralTriangle (GTE res g) -> blenderTriangularSurface res g

                    objVar <.> "show_transparent" .= True

                stmts2 = handleTriLabel ba tcec ecvc triLblGrpName t


        
handleTriLabel :: (CONSTRAINTS(s)) => 

        Blenderable s 
    ->  TrianglesContainingEdge_Cache (Ed s) (Tri s) 
    ->  EdgesContainingVertex_Cache (Vert s) (Ed s) 
    ->  BlenderGroupVarName -> Tri s -> Python ()

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
                                    : map mkGrpVarName 
                                        (bfi_groups . ba_faceInfo ba . triToAnySimplex2 $ t)


                    in do
                        newTextObj text
                        objVar <.> matrix_basis .= m
                        objCommon 
                            groups 
                            (text ++ " on "++show perm++" "++unFaceName (ba_faceName ba asi))
                            triLabelMat 


    where
        vs = faces20Ascending (pr_ds (ba_pr ba)) t
        cvs = map3 (ba_coords ba tcec ecvc) vs

        asi = triToAnySimplex2 t


        
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
    mesh (Mesh [p0,p1,p2] [(0,1,2)] False [])


safeOrthogonal :: Mat3 -> Proj4
safeOrthogonal m = 
    assert (matrixApproxEq (m .*. transpose m) idmtx) $
    assert (matrixApproxEq (transpose m .*. m) idmtx) $
    linear m


newGroup :: BlenderGroupVarName -> String -> Python ()
newGroup varName groupName = do
    varName .= methodCallExpr1 (dat "groups") "new" (str groupName) <.> "objects" 


testBlender :: (CONSTRAINTS0(s)) => Scene s -> IO ExitCode
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

data Mesh = Mesh {
        meshVertices :: [MeshVertex],
        meshFaces :: [Triple MeshVertexIndex],
        meshSmooth :: Bool,
        uv_textures :: [MeshTextureFaceLayer]
    }

type UV = Vec2

newtype MeshTextureFaceLayer = MeshTextureFaceLayer [Triple UV] 

type MeshVertexIndex = Int
type MeshVertex = Vec3

mesh :: Mesh -> Python ()
mesh m =
    do 
        meshVar .= (methodCallExpr1 (dat "meshes") "new" (str "M"))

        methodCall meshVar "from_pydata" (meshVertices m,emptyList,meshFaces m)

        when (meshSmooth m)
            (foreachStmt "f" (meshVar <.> "faces") (\f ->
                f <.> "use_smooth" .= True))

        forM_ (uv_textures m)
            (\(MeshTextureFaceLayer uvs) -> do
                meshTextureFaceLayerVar .= methodCallExpr (meshVar <.> "uv_textures") "new" ()
                methodCall
                    (meshTextureFaceLayerVar <.> "data") 
                    "foreach_set"
                    ( str "uv"
                    , concatMap ((++[-1,-1]) . concatMap asList . asList) uvs ))



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

data BlenderCurve = BlenderCurve {
    bevel_depth :: Double,
    -- | 0 to 32
    bevel_resolution :: Int,
    curve_spline :: BlenderSpline
}

data BlenderSpline = Nurbs {
    spline_points :: V.Vector SplinePoint,
    use_endpoint_u :: Bool,
    order_u :: Int,
    use_cyclic_u :: Bool
}

type SplinePoint = Vec3

extendVec34 :: Vec3 -> Vec4
extendVec34 = extendWith 1

newCurveObj :: BlenderCurve -> Python ()
newCurveObj BlenderCurve{..} = do
    curveVar .= methodCallExpr (dat "curves") "new" (str "Cu", str "CURVE")
    curveVar <.> "dimensions" .= str "3D" 
    curveVar <.> "bevel_depth" .= bevel_depth
    curveVar <.> "bevel_resolution" .= bevel_resolution
    curveVar <.> "use_fill_front" .= False
    curveVar <.> "use_fill_back" .= False
    --    curveVar <.> "fill_mode" .= str "half"

    case curve_spline of
         Nurbs{..} -> do
            splineVar .= methodCallExpr1 (curveVar <.> "splines") "new" (str "NURBS")
            methodCall (splineVar <.> "points") "add" 
                (SingleArg (V.length spline_points-1))
                -- -1 because the new spline already has one point
            methodCall (splineVar <.> "points") "foreach_set" (str "co",
                concatMap (asList . extendVec34) (V.toList spline_points))
            splineVar <.> "order_u" .= order_u
            splineVar <.> "use_endpoint_u" .= use_endpoint_u
            splineVar <.> "use_cyclic_u" .= use_cyclic_u
            splineVar <.> "use_smooth" .= True
    
    newObj "Cu" curveVar
    

-- | Returns the object in the 'objVar' \"register\" :-)
newObj ::  ToPython t => String -> t -> Python ()
newObj name objData = do
            objVar .= (methodCallExpr (dat "objects") "new" (str name,objData))
            methodCall1 (sceneVar <.> "objects") "link" objVar



--linkObjToScene =

grpLink :: BlenderGroupVarName -> Python () -> Python ()
grpLink gr obj = methodCall1 gr "link" obj


materialToBlender :: Material -> Python ()
materialToBlender m@Material{..} = do

    let var = ma_var m

    var .= methodCallExpr1 (dat "materials") "new" (str ma_name)

    let allProps = ("use_transparent_shadows" & True)
                        : ma_props
                        ++ Fold.concatMap transparency ma_transparency 

    mapM_ (setProp' var) allProps





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


blenderCurvedEdge :: Int -> Double -> (UnitIntervalPoint -> Vec3) -> Either String (Python ())
blenderCurvedEdge (steps :: Int) thickness f =
    let
        m = steps - 1

        ys = V.generate steps (\i -> f (fi i / fi m))

    in return $ do
        
        newCurveObj (BlenderCurve {
            bevel_depth = thickness,
            bevel_resolution = 10,
            curve_spline = Nurbs {
                use_endpoint_u = True,
                order_u = 3,
                spline_points = ys,
                use_cyclic_u = normsqr (f 0 - f 1) < 1E-10
            }
         })



        --sequence_ <$> zipWithM (\x y -> cylinder x y thickness) ys (tail ys)
            

blenderTriangularSurface :: Int 
    -> (Vec2 -> Vec3) 
        -- ^
        --
        -- Input is in the convex hull of 
        -- @{ Vec2 0 0, Vec2 1 0, Vec2 0 1 }@
    
    -> Python ()
blenderTriangularSurface = memo prepare
    where
        prepare steps = 
            let
                m = steps - 1
                nu = nuFromDistinctList vertices_intUV 
                toi = toInt nu

                (vertices_intUV,tris_intUV) = 
                    triangulateTriangle steps
                
                
                        
                tris = map (map3 toi) tris_intUV

                normalizeIntUV (u,v) = Vec2 (fi u / fi_m) (fi v / fi_m)

                vertices_normalizedUV = map normalizeIntUV vertices_intUV
                tris_normalizedUV = map (map3 normalizeIntUV) tris_intUV

                fi_m = fi m
            in
                \f -> mesh
                        (Mesh {
                            meshVertices = map f vertices_normalizedUV,

                            meshFaces = tris,
                            meshSmooth = True,
                            uv_textures = 
                                [ MeshTextureFaceLayer tris_normalizedUV ] 

                            }
                            
                            )
                


viewSelectedStmt :: Python ()
viewSelectedStmt = methodCall (ops "view3d") "view_selected" ()

deselectAllStmt :: Python ()
deselectAllStmt = foreachStmt "o" (py "context.selected_objects") 
    (\o -> o <.> "select" .= False)




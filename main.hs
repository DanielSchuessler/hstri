{-# LANGUAGE PackageImports, TemplateHaskell #-}
import Control.Applicative
import Control.Monad.Trans
import FRP.Elerea.Param
import Graphics.UI.GLFW as GLFW


--import Paths_lambdacube_examples (getDataFileName)


import Command
import Control.Arrow
import Control.Exception(assert)
import Control.Monad
import Data.List((\\),delete)
import Data.Vect.Double
import Data.Vect.Double.Base
import Data.Vect.Double.Instances
import Data.Vect.Double.OpenGL
import Data.Vect.Double.Util.Dim4
import Floor
import Graphics.Rendering.OpenGL  hiding(multMatrix)
import MathUtil
import ReadInput
import Utils
import SimplicialComplex
import TupleTH


getDataFileName :: [Char] -> IO [Char]
getDataFileName = return . ("/h/dev/tet-deform/" ++)

width :: Float
width = 150

height :: Float
height = 150

zNear ::  GLdouble
zNear = 0.001

zFar ::  GLdouble
zFar = 10000

main :: IO ()
main = do
    windowSize <- initCommon "Deformat0r"

    (mousePosition,mousePositionSink) <- external (zero :: MousePosition)
    (_mousePress,mousePressSink) <- external False
    (fblrPress,fblrPressSink) <- external noCommands

    initGL

    s <- fpsState
    mp <- uncurry MousePosition <$> getMousePosition
    sc <- start $ makeToplevelSignal windowSize mousePosition fblrPress mp
    driveNetwork sc (readInput s mousePositionSink mousePressSink fblrPressSink)

    closeWindow

--lightPos = Vertex4 2 5 (-4) 1
lightPos = let r = sqrt 2 in Vertex4 0 r r 0

setFOV fov = do
    matrixMode $= Projection
    loadIdentity
    (w,h) <- getWindowDimensions
    perspective fov (fromIntegral w/fromIntegral h) zNear zFar
    matrixMode $= Modelview 0

initGL = do
    depthFunc $= Just Less

    setFOV 20


    loadIdentity

    lighting $= Enabled
    colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
    light (Light 0) $= Enabled
    ambient (Light 0) $= Color4 0 0 0 1
    diffuse (Light 0) $= Color4 1 1 1 1
    specular (Light 0) $= let r = 0.5 in Color4 r r r 1
    lightModelAmbient $= let a=0.1 in Color4 a a a 1
    lightModelLocalViewer $= Enabled
    --lightModelColorControl $= SeparateSpecularColor

    lineSmooth $= Enabled
    blendFunc $= (SrcAlpha,OneMinusSrcAlpha)

    
type WindowSize = (Int,Int)

makeToplevelSignal :: ()
      => Signal WindowSize
      -> Signal MousePosition
      -> Signal Commands
      -> MousePosition
      -> SignalGen Time (Signal (IO ()))
makeToplevelSignal windowSize mousePosition commands initialMP = do
    time <- stateful 0 (+)
    last2 <- transfer (initialMP,initialMP) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\(o,n) -> n &- o) <$> last2
    --let mouseMove = mousePosition
    cam <- cameraSignal initialCamPos mouseMove commands windowSize
    zoom <- zoomSignal 45 commands

    let pausePressed = commanded Pause <$> commands

    animPlaying <- transfer True (\_ (a,b) c -> if a && b then not c else c) =<< withChangeDetect pausePressed 

    animTime <- transfer 0 (\dt playing prev -> if playing then prev + dt else prev) animPlaying

    return $ drawGLScene <$> windowSize <*> cam <*> animTime <*> zoom

initialCamPos ::  Vec3
initialCamPos = Vec3 (-4) 1 1

type FOV = GLdouble

zoomSignal :: FOV -> Signal Commands -> SignalGen Time (Signal FOV)
zoomSignal initial commands =
    let
        rate :: FOV
        rate = 10
    in
        transfer initial (\dt cmds prev ->
            case (commanded ZoomIn cmds, commanded ZoomOut cmds) of
                (True,False) -> prev - realToFrac dt*rate
                (False,True) -> prev + realToFrac dt*rate
                _ -> prev)
                         commands


drawGLScene :: ()
            => (Int, Int)
            -> CameraSignalState
            -> Time
            -> FOV
            -> IO ()
drawGLScene (w,h) css@(CameraSignalState cam dir up _) time fov = do
    clear [ColorBuffer,AccumBuffer,StencilBuffer,DepthBuffer]

    setFOV fov

    loadIdentity

    --print css

    lookAt (fmap glflt $ vec3ToVertex cam) 
           (fmap glflt . vec3ToVertex $ cam &+ dir) 
           (fmap glflt $ vec3ToVector up)

    position (Light 0) $= lightPos
    
    drawFloor



    renderGeom time


    --drawLight

    liftIO $ swapBuffers



renderGeom time = do

    let mat k0 k1 = do
            materialShininess FrontAndBack $= 30
            materialSpecular  FrontAndBack $= let r = 0.05 in Color4 r r r 1
            case (k0,k1) of
                (Edge,Edge) -> color (Vec3 1 1 1)
                (Face i,Face j) -> assert (i==j) $ faceMat i
                (Face i,Edge) -> faceMat i
                (Edge,Face i) -> faceMat i


        faceMat i = case i of
                     0 -> color (Vec3 1 0 0)
                     1 -> color (Vec3 0 1 0)
                     2 -> color (Vec3 0 0 1)
                     3 -> color (Vec3 1 1 0)


    
    let renderOpaques = do
            mat Edge Edge
            forM_ (edgeCoords theComplex)
                            (\(v0,v1) -> cylinder v0 v1)


    let renderTransparents = do
                renderPrimitive LineStrip $
                    forM_ (triCoords theComplexFaceSubdivided)
                        renderTriangle

    renderOpaques

    color (Vec3 0 0.6 0)
    materialSpecular FrontAndBack $= Color4 1 1 1 1
    materialShininess FrontAndBack $= 50
    shadeModel $= Flat
    
    renderTransparents

--     withBlend $ do
-- 
--         withoutLighting $ do
--             color (Vec4 0 0.6 0 0.2) 
--             renderTransparents
-- 
-- 
--         withDepthFuncEqual $ do
--             color (Vec4 0 0 0 0)
--             materialSpecular FrontAndBack $= Color4 1 1 1 1
--             materialShininess FrontAndBack $= 120
--             renderTransparents


withoutLighting a = do
    lighting $= Disabled
    a
    lighting $= Enabled

withBlend a = do
    blend $= Enabled
    a
    blend $= Disabled

withDepthFuncEqual a = do
    depthFunc $= Just Equal
    a
    depthFunc $= Just Less


renderTriangle = void . $(sequenceTuple 3) . $(mapTuple 3) vertex


renderTriangleSD vs = {-# CORE "renderTriangle" #-} renderPrimitive Triangles $ go 2 vs
    where
        go :: Int -> (Vec3,Vec3,Vec3) -> IO ()
        go 0 vs' = renderTriangle vs'
        go n vs'@(v0,v1,v2) = do
                g v0 v01
                g v01 v1
                g v1 v12
                g v12 v2
                g v2 v20
                g v20 v0
            where
                c = barycenter (Bary2 vs')
                (v01,v12,v20) = ($(mapTuple 3) (barycenter . Bary1) . $(subtuples 3 2)) vs'
                g u0 u1 = go (pred n) (c,u0,u1)



--     forM_ sampleLines
--         (\sampleLine -> 
--             cylinderStrip mat (fmap (second (theEmb time)) sampleLine))

theComplex = bary3 AbstractTet

theComplexFaceSubdivided = bary2 . bary2 $ theComplex

-- --  cylinderstrip test
--     cylinderStrip (\_ _ -> return ())
--         [ ((),  Vec3 (cos r) (sin r) r) | r <- [0,0.1..10] ]
        



drawLight = do
    pointSize $= 10
    renderPrimitive Points $ do
        color (Vec3 1 1 0)
        vertex lightPos





type Embedding = Vec4 -> Vec3

-- -- | Plain tetrahedron
-- theEmb1 (Vec4 x0 x1 x2 x3) = x0 *& Vec3 1 1 1 &+ Vec3 x1 x2 x3
-- 
-- theEmb2 cleft (Vec4 x0 x1 x2 x3) = v_xy  &+ (x2-x3) *& vec3Z
--     where
--         a = x0 + x1 
--         phi_min = cleft
--         phi_max = 2*pi - cleft
--         phi = phi_min + (phi_max-phi_min) * x0/a
--         v_xy | a > 0 = a *& Vec3 (- (sin phi)) (cos phi) 0
--              | otherwise = zero
-- 
-- 
-- theEmb3 = stereograph . to3Sphere
-- 
-- --theEmb _ = theEmb1
-- --theEmb t p = aff (theEmb1 p) (theEmb3 p) ((sin (t/5) + 1)/2)
-- -- theEmb ::  Time -> Vec4 -> Vec3
-- -- theEmb t = theEmb2 (pi*(sin (t/5) + 1)/2)
-- 
-- theEmb t = stereograph' t . to3Sphere

data SamplePointKind = Face FaceNr | Edge
    deriving Show
type SamplePoint = (SamplePointKind,Vec4)

sampleLines :: [[SamplePoint]]
sampleLines = 
        [
            [ 
                 (pointKind, point)

              | let rest = nm-i,
                j <- [0..rest],
                let k = rest - j 
                    pointKind = if i*j*k == 0 then Edge else Face faceNr 
                    point = 
                          (fi i *& u &+ 
                           fi j *& v &+
                           fi k *& w) 

                            &* (recip (fi nm)) 
                    
                    
            ]
               
            
            | (faceNr,(u0,v0,w0)) <- faces,
              (u,v,w) <- [(u0,v0,w0),(v0,w0,u0),(w0,u0,v0)],
              i <- [0,n..nm]
        ]
    where
        n = 30
        m = 4
        nm = n*m
            

vert ::  Int -> Vec4
vert i = [vec4X,vec4Y,vec4Z,vec4W] !! i

type FaceNr = Int

faces :: [(FaceNr,(Vec4,Vec4,Vec4))]
faces = [ (faceNr,(vert l,vert m,vert n))
            | 
            faceNr <- [0..3],
            let [l,m,n] = delete faceNr [0..3]
            
          ] 


type Material = IO ()

cylinderStrip :: (a -> a -> Material) -> [(a,Vec3)] -> IO ()
cylinderStrip material ((k0,v0):ps@((k1,v1):_)) = do
        material k0 k1
        cylinder v0 v1 
        cylinderStrip material ps 


cylinderStrip _ _ = return ()


cylinder v0 v1 = preservingMatrix $ do
            let diff = v1 &- v0
                norm_diff = norm diff
                diff' = diff &/ norm_diff
                v = anyOrth diff'

            glTranslate v0
            multMatrix (Mat3 v (crossprod v diff') diff')
            renderQuadric sty (Cylinder radius radius (realToFrac $ norm_diff ) slices stacks)

    where
        slices = 6
        stacks = 1
        sty = QuadricStyle (Just Smooth) NoTextureCoordinates Outside FillStyle 
        radius = 0.005

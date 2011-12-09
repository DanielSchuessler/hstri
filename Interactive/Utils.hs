{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# OPTIONS -Wall #-}
module Interactive.Utils where

import Control.Monad
import Control.Monad.Trans
import Data.IORef

import FRP.Elerea.Param
import Graphics.UI.GLFW as GLFW

import Data.Vect.Double
import Graphics.Rendering.OpenGL.GLU.Errors
import Graphics.Rendering.OpenGL(get)
import Data.Tensor
import Command
import Data.BitSet
import Control.Applicative

-- Reactive helper functions

integral :: (Real p, Fractional t) => t -> Signal t -> SignalGen p (Signal t)
integral v0 s = transfer v0 (\dt v v1 -> v1+v*realToFrac dt) s

driveNetwork :: (MonadIO m) => (p -> IO (m a)) -> IO (Maybe p) -> m ()
driveNetwork network driver = loop
    where
        loop = do
            dt <- liftIO driver
            case dt of
                Just dt' -> do
                    _ <- join . liftIO $ network dt'
                    loop
                Nothing -> return ()

-- OpenGL/GLFW boilerplate

initCommon :: String -> IO (Signal (Int, Int))
initCommon title = do
    _ <- initialize
    _ <- openWindow defaultDisplayOptions
        { displayOptions_numRedBits     = 8
        , displayOptions_numGreenBits   = 8
        , displayOptions_numBlueBits    = 8
        , displayOptions_numDepthBits   = 24
        , displayOptions_numFsaaSamples = Just 2
--         , displayOptions_displayMode = Fullscreen
--         , displayOptions_width = 1280
--         , displayOptions_height = 800
        }
    setWindowTitle title

    (windowSize,windowSizeSink) <- external (0,0)
    setWindowSizeCallback $ \w h -> do
        windowSizeSink (fromIntegral w, fromIntegral h)

    
    setWindowBufferSwapInterval 1

    return windowSize

-- FPS tracking

data State = State { frames :: IORef Int, t0 :: IORef Double }

fpsState :: IO State
fpsState = do
    a <- newIORef 0
    b <- newIORef 0
    return $ State a b

updateFPS :: State -> Double -> IO ()
updateFPS state t1 = do
    let t = 1000*t1
        fR = frames state
        tR = t0 state
    modifyIORef fR (+1)
    --frames state $~! (+1)
    t0' <- readIORef tR
    writeIORef tR $ t0' + t
    --t0' <- get (t0 state)
    --t0 state $= t0' + t
    when (t + t0' >= 5000) $ do
    f <- readIORef fR --get (frames state)
    let seconds = (t + t0') / 1000
        fps = fromIntegral f / seconds
    putStrLn (show f ++ " frames in " ++ show seconds ++ " seconds = "++ show fps ++ " FPS")
    writeIORef tR 0 --t0 state $= 0
    writeIORef fR 0 --frames state $= 0

-- Continuous camera state (rotated with mouse, moved with arrows)

data CameraSignalState = CameraSignalState Vec3 Vec3 Vec3 MousePosition
    deriving Show

data MousePosition = MousePosition Int Int
    deriving Show

instance AbelianGroup MousePosition where
    MousePosition x y &+ MousePosition x' y' = MousePosition (x+x') (y+y')
    MousePosition x y &- MousePosition x' y' = MousePosition (x-x') (y-y')
    zero = MousePosition 0 0
    neg x = zero &- x

cameraSignal :: Real t => Vec3 -> Signal MousePosition
             -> Signal Commands
             -> Signal (Int,Int)
             -> SignalGen t (Signal CameraSignalState)
cameraSignal p mposs keyss = transfer3 (CameraSignalState p zero zero zero) calcCam mposs keyss
  where
    calcCam 
        dt 
        dmp 
        commands
        (windowWidth,windowHeight)
        (CameraSignalState p0 _ _ mp) 
        
            = CameraSignalState p' dir up mp'

      where
        f0 c n = if member c commands then (&+ n) else id
        p'  = foldr ($) p0   [  f0 CLeft (v &* (-t)),
                                f0 Forward (dir &* t),
                                f0 Backward (dir &* (-t)),
                                f0 CRight (v &* t),
                                
                                f0 Down (up &* (-t)),
                                f0 Up (up &* t) 
                                
                             ] 
                            
                            

        k   = if member Turbo commands then 6 else 0.5
        t   = k * realToFrac dt
        mp'@(MousePosition mx' my') = dmp &+ mp
        rm  =   
        
                rotMatrixY (4*pi*fi mx' / fromIntegral windowWidth) .*. 
                rotMatrixX (4*pi*fi my' / fromIntegral windowHeight)

        dir = rm *. (neg vec3Z)
        up  = rm *. vec3Y
        v   = normalize $ dir &^ up
            


printErrors :: IO ()
printErrors = do
    es <- get errors
    let p e = putStrLn ("*** ERROR: "++show e)
    mapM_ p es


aff :: Vector g => g -> g -> Double -> g
aff a b p = (1-p) *& a &+ p *& b


fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

vec3ToVertex ::  Vec3 -> Vertex3 Double
vec3ToVertex (Vec3 x y z) = Vertex3 x y z
vec3ToVector ::  Vec3 -> Vector3 Double
vec3ToVector (Vec3 x y z) = Vector3 x y z

type Time = Double


withChangeDetect :: Eq a => Signal a -> SignalGen p (Signal (Bool, a))
withChangeDetect s = do
    sprev <- delay undefined s
    isFirst <- delay True (return False)
    let f xprev x isFirst' = (isFirst' || x /= xprev, x)
    return (f <$> sprev <*> s <*> isFirst)


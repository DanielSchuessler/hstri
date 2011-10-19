module ReadInput where

import Data.Vect.Double
import Graphics.Rendering.OpenGL
import Utils
import Control.Monad
import Data.Vect.Double.OpenGL
import Command
import Graphics.UI.GLFW


clampMiddle :: Int -> Int -> Int
clampMiddle x w =
    let w4 = div w 4
    in  w4 + mod (x-w4) (2*w4)

readInput :: State
          -> (MousePosition -> IO a)
          -> (Bool -> IO b)
          -> (Commands -> IO c)
          -> IO (Maybe Time)
readInput s mousePos mouseBut fblrPress = do
    printErrors

    t <- getTime
    resetTime

    (x,y) <- getMousePosition
    mousePos (MousePosition x y)
    (w,h) <- getWindowDimensions
    let x' = clampMiddle x w 
        y' = clampMiddle y h

    when ((x,y) /= (x',y')) (setMousePosition x' y')


    --setMousePosition 10 10

    mouseBut =<< mouseButtonIsPressed MouseButton0
    fblrPress =<< getCommands

    updateFPS s t
    k <- keyIsPressed KeyEsc
    return $ if k then Nothing else Just t


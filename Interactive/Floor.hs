
module Interactive.Floor where

import Data.Vect.Double
import Graphics.Rendering.OpenGL
import Interactive.Utils
import Control.Monad
import Data.Vect.Double.OpenGL

drawFloor = do
    let floorTileSize = 1
        n = 25
        floorColor1 = let r=0.5 in Vec4 r r r 1
        floorColor2 = Vec4 0 0 0 1

        floorCenter = neg vec3Y -- &+ Vec3 40 0 40
        v i j = do
            normal (Normal3 0 1 0 :: Graphics.Rendering.OpenGL.Normal3 GLdouble)
            vertex (floorTileSize *& (Vec3 (fi (i - n)) 0 (fi (j-n))) &+ floorCenter)

    shadeModel $= Smooth
    materialSpecular  FrontAndBack $= Color4 0.5 0.5 0.5 1
    materialShininess FrontAndBack $= 40
    materialEmission  FrontAndBack $= Color4 0 0 0 1

    forM_ [0..2*n-1]
        (\i -> do
            let jrange = [0..2*n-1]

            renderPrimitive Quads $ do
                forM_ jrange (\j -> do
                    color (if even i == even j then floorColor1 else floorColor2)
                    v i j
                    v (i+1) j
                    v (i+1) (j+1)
                    v i (j+1)
                    
                    ))


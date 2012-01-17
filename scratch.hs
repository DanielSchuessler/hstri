{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
import HsTri
import qualified Data.Vector as V
import DotUtil
import TriArcGraph
import EdgeCentered
import Data.Char
import Data.Vect.Double.Util.Dim3
import Data.Vect.Double.Base
import Tetrahedron.Vertex


f2 (Vec2 a b) = 
    let
        x = b-a -- [-1, 1]
        y = 2*(b+a)-1 -- [-1,1]
    in
           moeb (Vec2 (2*pi*x) y)



main = testBlender (setCams [oneTetCam] . defaultScene $ pseudomanifoldStyle pren)

pren :: PreRenderable (SC2 Vertex)
pren = 
    pr_setTriangleEmbedding (const . Just $ GTE 50 f2) $

    mkPreRenderable coords abstractTri


  where
    coords = undefined
--     coords A = zero
--     coords B = vec3X
--     coords Tetrahedron.Vertex.C = vec3Y
--     coords D = vec3Z

    g (Vec2 a b) = moeb (Vec2 (2*pi*a) (2*b-1))

f = moeb . h

h (Vec2 a b) = 
    case a+b of
            0 -> Vec2 0 0
            c ->

                case (b-a)/c of
                    
                    1 ->
                            Vec2 0 (-c)

                    d 
                        | d>=0 -> 
                        
                            Vec2 (2*pi*d) c

                        | otherwise ->

                            Vec2 (2*pi*(1+d)) (-c)




test_f =
    let
        steps = 5
        m = steps-1

        (verts,_) = triangulateTriangle steps
    in
        pr (prettyFunction f (map (uncurry Vec2 . (map2 (\x -> fi x / fi m))) verts)) 
                

                       
            
-- f(u,0) = f(0,u)
--

moeb (Vec2 u v) = 
            let
                r = 1 + 0.5 * v * cos(u/2)
              in
                Vec3 (r*cos u) (r*sin u) (0.5 * v * sin (u/2))

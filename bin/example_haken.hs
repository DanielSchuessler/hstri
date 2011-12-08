{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}
import ParseRga
import Data.Vect.Double hiding((.*))
import EdgeCentered
import Test.QuickCheck
import IndexedSimplices
import Data.Map as M
import MathUtil
import Blender
import Latexable
import Triangulation


main = do
    [tr] <- readRgaFile "/h/dev/regina-things/TxS1Triang.rga"

    putStrLn (toLatex tr)

    let oiedge = 0 ./ oedge (vA,vB) :: OIEdge

    putStrLn (listToLatex (tOIEdgeEquivalents tr oiedge))

    let (m,x) = makeEdgeNeighborhood tr oiedge
    mapM_ print . M.toList $ m 
--     quickCheck (prop_notTooManyGluings tr m)
--     quickCheck (prop_edgeIsGlued tr m oiedge)
--     quickCheck (prop_enoughGluings tr oiedge (m,x))


    let rot = withOrigin (Vec3 0.5 0.5 0.5) (rotate3 (pi*0.42) vec3Z)

    testBlender . 
        setCams cams .
       defaultScene $
    --   transformCoords rot $
       (pseudomanifoldStyle  x)

cams = [Cam (Vec3 (-0.22153916954994202) (-0.8245114088058472) (3.923537492752075))
            (eulerAnglesXYZ (0.20578281581401825) (-6.364510909406818e-07) (-0.11199047416448593))
            defaultFOV

       ,Cam (Vec3 0 (-0.83) 0)
            (eulerAnglesXYZ (pi/2) 0 0)
            1.9
       ]

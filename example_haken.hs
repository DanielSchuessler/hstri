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
import DD
import PrettyUtil

tr = fromRight $ mkTriangulation 6 [(0 ./ tBCD, 1 ./ oCBD), (0 ./ tACD, 2 ./ oCDB), (0 ./ tABD, 3 ./ oDCB), (0 ./ tABC, 4 ./ oCDB), (1 ./ tBCD, 0 ./ oCBD), (1 ./ tACD, 2 ./ oDCA), (1 ./ tABD, 3 ./ oCDA), (1 ./ tABC, 5 ./ oCDB), (2 ./ tBCD, 0 ./ oDAC), (2 ./ tACD, 1 ./ oDCA), (2 ./ tABD, 4 ./ oCDA), (2 ./ tABC, 5 ./ oDCA), (3 ./ tBCD, 0 ./ oDBA), (3 ./ tACD, 1 ./ oDAB), (3 ./ tABD, 5 ./ oBCA), (3 ./ tABC, 4 ./ oCBA), (4 ./ tBCD, 0 ./ oCAB), (4 ./ tACD, 2 ./ oDAB), (4 ./ tABD, 5 ./ oADB), (4 ./ tABC, 3 ./ oCBA), (5 ./ tBCD, 1 ./ oCAB), (5 ./ tACD, 2 ./ oCBA), (5 ./ tABD, 4 ./ oADB), (5 ./ tABC, 3 ./ oDAB)]


oiedge = 0 ./ oedge (vA,vB) :: OIEdge
spqwc = makeEdgeNeighborhood tr oiedge show

main = do
--     [tr'] <- readRgaFile "/h/dev/regina-things/TxS1Triang.rga"
--     print (tOriginalGluings tr == tOriginalGluings tr')
--     error ""

    putStrLn (toLatex tr)


    putStrLn (listToLatex (tOIEdgeEquivalents tr oiedge))

--    mapM_ print . M.toList $ m 
--     quickCheck (prop_notTooManyGluings tr m)
--     quickCheck (prop_edgeIsGlued tr m oiedge)
--     quickCheck (prop_enoughGluings tr oiedge (m,x))


    let rot = withOrigin (Vec3 0.5 0.5 0.5) (rotate3 (pi*0.42) vec3Z)

    testBlender . 
        setCams cams .
       defaultScene $ fromSpqwc spqwc
    --   transformCoords rot $

cams = [Cam (Vec3 (-0.22153916954994202) (-0.8245114088058472) (3.923537492752075))
            (eulerAnglesXYZ (0.20578281581401825) (-6.364510909406818e-07) (-0.11199047416448593))
            defaultFOV

       ,Cam (Vec3 0 (-0.83) 0)
            (eulerAnglesXYZ (pi/2) 0 0)
            1.9
       ]

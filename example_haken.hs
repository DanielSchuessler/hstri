{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}
import Codec.Rga.Parser
import Data.Vect.Double hiding((.*))
import EdgeCentered
import Test.QuickCheck
import Data.Map as M
import MathUtil
import Blender
import Latexable
import Triangulation
import VerboseDD
import PrettyUtil
import ExampleTriangulations


tr = spqwc_tr torus3

main = do
--     [tr'] <- readRgaFile "/h/dev/regina-things/TxS1Triang.rga"
--     print (tGluingsIrredundant tr == tOriginalGluings tr')
--     error ""

    putStrLn (toLatex tr)


    --putStrLn (listToLatex (tOIEdgeEquivalents tr oiedge))

--    mapM_ print . M.toList $ m 
--     quickCheck (prop_notTooManyGluings tr m)
--     quickCheck (prop_edgeIsGlued tr m oiedge)
--     quickCheck (prop_enoughGluings tr oiedge (m,x))


    let rot = withOrigin (Vec3 0.5 0.5 0.5) (rotate3 (pi*0.42) vec3Z)

    testBlender . 
        setCams cams .
       defaultScene $ fromSpqwc torus3
    --   transformCoords rot $

cams = [Cam (Vec3 (-0.22153916954994202) (-0.8245114088058472) (3.923537492752075))
            (eulerAnglesXYZ (0.20578281581401825) (-6.364510909406818e-07) (-0.11199047416448593))
            defaultFOV

       ,Cam (Vec3 0 (-0.83) 0)
            (eulerAnglesXYZ (pi/2) 0 0)
            1.9
       ]

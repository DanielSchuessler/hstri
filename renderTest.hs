{-# LANGUAGE ViewPatterns #-}

import HsTri
import Data.Vect.Double.Base
import ReadArgs

main = do
    (a,b) <- readArgs
    
    testBlender . defaultScene $
        (
        mkBlenderable pseudomanifoldStyle (pr_hideTris (const True) pren)
        `disjointUnion`
        mkBlenderable normalSurfaceStyle (disk a b)
--         `disjointUnion`
--         mkBlenderable normalSurfaceStyleB (disk 3 2)
        )


major = 2
minor = 0.8

pren = pr_hideEds he $
            
                pr_quad 30 
                (torusCoords major minor . torusBoundary)


  where
        he (unAsc2 -> (v0,v1)) = v0==(True,True)

disk α β = pr_quad (round (20 * max β 1 + 5 * α)) 
                (torusCoords major minor . 
                    mapSolidTorus (Mat2 (Vec2 1 β) (Vec2 0 α)) . 
                    holedMeridionalDisc) 


holedMeridionalDisc :: UnitSquare -> SolidTorusPoint
holedMeridionalDisc (Vec2 x y) = STP 0 (2*pi*x) (1-0.9*y)


{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, GADTs, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving, ExtendedDefaultRules #-} 


import Blender
import Data.Function
import Data.Vect.Double
import ExampleTriangulations
import Control.Monad
import StandardCoordinates
import Tetrahedron.NormalDisc
import Numeric.AD.Vector
import CheckAdmissibility
import NormalSurfaceBasic

spqwc = spqwc_oneTet 
tr = spqwc_tr spqwc


main :: IO ()
main = 
    forM_ 
    
       (map (toAdmissible stdCoordSys tr)
        (AnyStandardCoords (normalTriList (tindex 0))
            : fmap AnyStandardCoords (normalQuadList (tindex 0)) ))

    (\stc ->

    testBlender . 
       setCams [Cam (-2.8 *& vec3Y) ((pi/2) *& vec3X) defaultFOV] . 
       defaultScene .
       transformCoords rot $
       (fromSpqwcAndIntegerNormalSurface spqwc stc)
            
       

       
       )
        --`disjointUnion` abnormalCurve


rot :: FF Tup3 Tup3 Double
rot = liftVec3 . rotate3 (pi*0.05) vec3X . rotate3 (pi*0.42) vec3Z . tup3toVec3 . fmap realToFrac



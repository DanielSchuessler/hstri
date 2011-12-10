{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, GADTs, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving, ExtendedDefaultRules #-} 
{-# OPTIONS -Wall #-}


import Blender
import Data.Function
import Data.Vect.Double
import PreRenderable
import ExampleTriangulations
import Control.Monad
import StandardCoordinates
import NormalDisc
import DisjointUnion



main :: IO ()
main = 
    forM_ (standardCoordinates (normalTriList (tindex 0))
            : fmap standardCoordinates (normalQuadList (tindex 0))
    
            )

    (\stc ->

    testBlender . 
       setCams [Cam (-2.8 *& vec3Y) ((pi/2) *& vec3X) defaultFOV] . 
       defaultScene .
       transformCoords rot $
       (fromSPQWC spqwc_oneTet
        `disjointUnion` 
        fromStandardCoordinates spqwc_oneTet (stc :: StandardCoordinates Int)
       

       
       ))
        --`disjointUnion` abnormalCurve
  where
    rot = rotate3 (pi*0.05) vec3X . rotate3 (pi*0.42) vec3Z



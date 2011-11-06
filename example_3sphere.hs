{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, GADTs, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving #-} 
{-# OPTIONS -Wall #-}

import AbstractTetrahedron
import Blender
import Blenderable
import Data.Function
import Data.Vect.Double
import MathUtil
import SimplexLabels
import SimplicialComplex
import System.Exit
import Data.Monoid



main :: IO ExitCode
main = testBlender . 
       defaultScene .
       transformCoords rot $
       (pseudomanifold 
            (setGluing "F" (vA,vB,vC) (vA,vB,vD) mempty
                 (setGluing "G" (vA,vC,vD) (vB,vC,vD) mempty
       
                tet3d)))
        --`disjointUnion` abnormalCurve
  where
    rot = withOrigin (Vec3 0.5 0.5 0.5) (rotate3 (pi*0.42) vec3Z)



{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, GADTs, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving #-} 
{-# OPTIONS -Wall #-}

import Tetrahedron
import Blender
import Data.Function
import Data.Vect.Double
import SimplexLabels
import SimplicialComplex
import System.Exit



main :: IO ExitCode
main = testBlender . 
       defaultScene .
       transformCoords rot $
       (pseudomanifoldStyle 
            (setGluing "F" (vA,vB,vC) (vA,vB,vD) S3bac
                 (setGluing "G" (vA,vC,vD) (vB,vC,vD) S3cab
       
                tet3d)))
        --`disjointUnion` abnormalCurve
  where
    rot = withOrigin (Vec3 0.5 0.5 0.5) (rotate3 (pi*0.42) vec3Z)



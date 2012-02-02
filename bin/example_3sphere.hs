{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, GADTs, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving #-} 
{-# OPTIONS -Wall #-}

import Tetrahedron
import Blender
import Blender.Conversion
import Data.Function
import Data.Vect.Double
import MathUtil
import Simplicial.SimplicialComplex
import System.Exit
import Data.Monoid
import PreRenderable



main :: IO ExitCode
main = testBlender . 
       defaultScene .
       transformCoords rot $
       (pseudomanifoldStyle 
            (setGluing "F" (vA,vB,vC) (vA,vB,vD) mempty
                 (setGluing "G" (vA,vC,vD) (vB,vC,vD) mempty
       
                tet3d)))
        --`disjointUnion` abnormalCurve
  where
    rot = withOrigin (Vec3 0.5 0.5 0.5) (rotate3 (pi*0.42) vec3Z)



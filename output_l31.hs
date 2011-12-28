{-# LANGUAGE TupleSections, ExtendedDefaultRules #-}
import ExampleTriangulations
import NormalEverything
import Element
import Blenderable
import Blender
import DisjointUnion
import ConcreteNormal
import Simplicial.DeltaSet
import Simplicial.AnySimplex
import TriangulationCxtObject
import HomogenousTuples
import Data.Function
import PrettyUtil
import Data.Vect.Double.Base
import Latexable
import EqvGraphs
import QuadCoordinates




tr = tr_l31
spqwc = spqwc_l31

smes :: IO ()
smes = putStr (latexifyStandardMatchingEquations tr)


main = (testBlender . setCams [twoTetCam])
        (defaultScene $
            (
                (fromSPQWC spqwc)
--             `disjointUnion`
   --             fromStandardCoordinates spqwc (standardCoordinates [0./Q_ac,1./Q_ac,2./Q_ad,3./Q_ad])
            )
        )
    

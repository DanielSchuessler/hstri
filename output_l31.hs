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




tr = tr_l31
spqwc = spqwc_l31




main = (testBlender . setCams [octahedronCam1])
        (defaultScene
            (
                (fromSPQWC spqwc)
   --             `disjointUnion`
   --             fromStandardCoordinates spqwc (standardCoordinates [0./Q_ac,1./Q_ac,2./Q_ad,3./Q_ad])
            )
        )
    

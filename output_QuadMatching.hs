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

n = 4

vTop = (tr_aroundEdge_topVertex n)
vBot = (tr_aroundEdge_bottomVertex n)


tr = tr_aroundEdge n
spqwc = spqwc_aroundEdge n


linkBot = vertexLinkingSurface vBot
linkTop = vertexLinkingSurface vTop

is = [ 0.. 3]

upQuads = standardCoordinates [i./Q_ac | i <- is]
downQuads = standardCoordinates [i./Q_ad | i <- is]

irrelevants = 
    sumV
    (standardCoordinates [i./Q_ab | i <- is]
     :
     [ vertexLinkingSurface (pMap tr (i ./ vC)) | i <- is]) 


main = (testBlender . setCams [octahedronCam1])
        (defaultScene
            (
                (fromSPQWC spqwc)
                `disjointUnion`
                fromStandardCoordinates spqwc upQuads
            )
        )
    

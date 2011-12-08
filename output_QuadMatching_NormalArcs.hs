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


addFilter ba = ba {
    ba_visible =
        foldAnySimplexWithNat (\n x ->
                        caseNat2 n
                            True
                            -- drop normal arcs which don't enclose the top or bottom vertex
                            (case x of
                                  OT cncs ->
                                        elem2 
                                            (pMap tr (forgetVertexOrder
                                                tr_aroundEdge_centralEdge_preimage))
                                            (map2 (pMap tr .
                                                        iNormalCornerGetContainingEdge . c_type)
                                                cncs))

                                      
                            -- drop normal discs
                            (const False))
 }

linkBot = vertexLinkingSurface vBot
linkTop = vertexLinkingSurface vTop

main = (testBlender . setCams [octahedronCam1])
        (defaultScene
            (
                (fromSPQWC spqwc)
                `disjointUnion`
                (fromStandardCoordinates spqwc (standardCoordinates (0./Q_ab)) )
--                 (addFilter $ fromStandardCoordinates spqwc linkTop)
--                 `disjointUnion`
--                 (addFilter $ fromStandardCoordinates spqwc linkBot)
--                 `disjointUnion`
--                 (addFilter $ fromStandardCoordinates spqwc (linkTop ^+^ linkBot))
            )
        )
    

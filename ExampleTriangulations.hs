{-# OPTIONS -Wall #-}
module ExampleTriangulations where

import TriangulationCxtObject
import EdgeCentered
import Blenderable
import Data.Vect.Double

tr_aroundEdge :: Word -> Triangulation
tr_aroundEdge n =
 fromRight $ mkTriangulation [0..tindex (n-1)]
    [ (tindex i ./ tABD,
       tindex (mod (i+1) n) ./ oABC) 
        
        | i <- [0..n-1]
    ]            

tr_octahedron :: Triangulation
tr_octahedron = tr_aroundEdge 4 

tr_aroundEdge_topVertex, tr_aroundEdge_bottomVertex :: Word -> TVertex
tr_aroundEdge_bottomVertex n = pMap (tr_aroundEdge n) (0 ./ vB)
tr_aroundEdge_topVertex n = pMap (tr_aroundEdge n) (0 ./ vA)

tr_aroundEdge_centralEdge_preimage :: OIEdge
tr_aroundEdge_centralEdge_preimage = 0 ./ oedge (vB,vA)

tr_aroundEdge_centralEdge :: Word -> T OIEdge
tr_aroundEdge_centralEdge n = pMap (tr_aroundEdge n) tr_aroundEdge_centralEdge_preimage

tr_octahedronWithGluing :: Triangulation
tr_octahedronWithGluing = fromRight $ addGluings tr_octahedron 
    [ oiTriangleGluing (0 ./ oCDA) (0 ./ oCDB) ]

spqwc_aroundEdge :: Word -> SPQWithCoords EdgeNeighborhoodVertex
spqwc_aroundEdge n = makeEdgeNeighborhood (tr_aroundEdge n) tr_aroundEdge_centralEdge_preimage 

octahedronCam1 :: Cam
octahedronCam1 =
    Cam
        (Vec3 0.217 (-3.091) 2.071)
        (Vec3 1.0077831745147705 0 0.06999944895505905)
        defaultFOV

tr_oneTet :: Triangulation
tr_oneTet = fromRight $ mkTriangulation [0] []

spqwc_oneTet :: SPQWithCoords Vertex
spqwc_oneTet = geometrifySingleTetTriang tr_oneTet

{-# LANGUAGE NoMonomorphismRestriction #-}
import Equivalence
import Triangulation
import ParseRga
import AbstractTetrahedron

data EdgeNeighborhoodVertex =
    Bottom |
    Top |
    Ring Int

makeEdgeNeighborhood tr oEdge =
    let
        oEdgeClass = eqvEquivalents (oEdgeEqv tr) oEdge
    in
        oEdgeClass
        

main = do
    [tr] <- readRgaFile "/h/dev/regina-things/sfsHakenExample2Triang.rga"
    print (makeEdgeNeighborhood tr (0 ./ oedge (vA,vB)))


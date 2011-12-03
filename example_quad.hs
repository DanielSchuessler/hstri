import Blender
import TriangulationCxtObject
import NormalDisc
import INormalDisc
import NormalCoordinates
import Triangulation
import EdgeCentered
import Data.Vect.Double
import Simplicial.Labels

exampleTri = fromRight $ mkTriangulation [0..3]
    [ (tindex i ./ tABD,tindex (mod (i+1) 4) ./ oABC) 
        
        | i <- [0..3]
    ]

(enMap,en) = makeEdgeNeighborhood exampleTri (0 ./ oedge (vA,vB))

main = testBlender
    (defaultScene 
        (transformCoords ((diag (Vec3 1 1 1.4) :: Mat3) Data.Vect.Double.*.) 
            (pseudomanifold en)))

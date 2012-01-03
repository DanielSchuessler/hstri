import NormalEverything
import ConcreteNormal.PreRenderable
import Blender
import Triangulation
import SimplicialPartialQuotient
import DisjointUnion


tr = mkTriangulation 1 [ (0 ./ tABC, 0 ./ oDAB) ]


spqwc = geometrifySingleTetTriang tr show


ntA' = standardCoordinates (0 ./ ntA)
ntB' = standardCoordinates (0 ./ ntB)
ntC' = standardCoordinates (0 ./ ntC)
ntD' = standardCoordinates (0 ./ ntD)

q_ab' = standardCoordinates (0 ./ Q_ad)

ns = 2 *^ sumV [ ntA', ntB', ntC', ntD'
            
            ]

main = testBlender
        (defaultScene
            (fromSPQWC spqwc
                `disjointUnion`
             fromStandardCoordinatesInteger spqwc ns)) 
                
        

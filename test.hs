import Test.QuickCheck.Gen
import AbstractTetrahedron
import System.Random
import Test.QuickCheck
import StandardCoordinates
import NormalDisc
import Triangulation
import TriangulationCxtObject
import QuadCoordinates


main = do
    let go a = do
        res <- a
        print res
        return res

    res <- mapM go [ 
        qc_Vertex, 
        qc_Edge, 
        qc_Triangle, 
        qc_AbstractTetrahedron, 
        qc_StandardCoordinates, 
        qc_NormalArc, 
        qc_NormalDisc, 
        qc_Triangulation, 
        qc_TriangulationCxtObject,
        qc_Equivalence,
        qc_Util,
        qc_QuadCoordinates
        ]
    print (and res)

import Test.QuickCheck.Gen
import AbstractTetrahedron
import System.Random
import Test.QuickCheck
import StandardCoordinates
import NormalDisc
import Triangulation.Tests
import TriangulationCxtObject
import QuadCoordinates
import AbstractNeighborhood
import MathUtil
import VerboseDD


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
        qc_Equivalence,
        qc_Util,
        qc_QuadCoordinates,
        qc_AbstractNeighbordhood,
        qc_MathUtil,
        qc_FacetGluing,
        qc_VerboseDD
        ]
    print (and res)

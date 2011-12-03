import Test.QuickCheck.Gen
import AbstractTetrahedron
import System.Random
import Test.QuickCheck
import NormalCoordinates
import NormalDisc
import Triangulation
import TriangulationCxtObject


main = do
    let go a = do
        res <- a
        print res
        return res

    res <- mapM go [ 
        qc_AbstractTetrahedron, 
        qc_NormalCoordinates, 
        qc_NormalArc, 
        qc_NormalDisc, 
        qc_Triangulation, 
        qc_TriangulationCxtObject,
        qc_Equivalence 
        ]
    print (and res)

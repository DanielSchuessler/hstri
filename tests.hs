{-# OPTIONS -Wall #-}
import AbstractNeighborhood
import AbstractTetrahedron
import MathUtil
import Tetrahedron.NormalDisc
import QuadCoordinates
import StandardCoordinates
import Triangulation.Tests
import TriangulationCxtObject
import VerboseDD
import Math.Groups.Tests
import Tetrahedron.Tests
import Util
import Orientation.Tests
import Control.Monad
import System.Exit


tests :: [IO Bool]
tests = [ 
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
        qc_VerboseDD,
        qc_Math_Groups_Tests,
        qc_Tetrahedron_Tests,
        qc_Orientation
        ]


go :: IO Bool -> IO ()
go a = do
        res <- a
        unless res $ do
            putStrLn "A test failed."
            exitWith (ExitFailure 1)
        

main :: IO ()
main = do

    mapM_ go tests

    putStrLn "All tests passed."


{-# OPTIONS -Wall #-}
import MathUtil
import QuadCoordinates.Tests
import Triangulation.Tests
import TriangulationCxtObject
import Math.Groups.Tests
import Tetrahedron.Tests
import Util
import Orientation.Tests
import Control.Monad
import System.Exit
import Tests
import Equivalence.Tests


tests :: [IO Bool]
tests = [ 
        qc_Triangulation, 
        qc_Equivalence,
        qc_Util,
        qc_QuadCoordinates,
        qc_MathUtil,
        qc_Tests,
        qc_Math_Groups_Tests,
        qc_Tetrahedron,
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


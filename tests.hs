{-# OPTIONS -Wall #-}
import Control.Monad
import Equivalence.Tests
import Math.Groups.Tests
import Math.GaussElim.Tests
import Orientation.Tests
import QuadCoordinates.Tests
import System.Exit
import Tests
import Tetrahedron.Tests
import Triangulation.Tests
import Util


tests :: [IO Bool]
tests = [ 
        qc_Tests,
        qc_Triangulation, 
        qc_Equivalence,
        qc_Util,
        qc_QuadCoordinates,
        qc_GaussElim,
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


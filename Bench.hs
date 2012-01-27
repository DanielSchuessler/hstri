{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Bench where
import Triangulation.Random
import Test.QuickCheck
import System.Random
import Control.DeepSeq
import VerboseDD
import Triangulation
import Test.QuickCheck.Gen
import System.Random
import Triangulation.Class


qVertexSolBench :: ToTriangulation a => [a] -> [Int]
qVertexSolBench tr = map (length' . asList . qVertexSolutions) tr

length' xs = xs `deepseq` length xs


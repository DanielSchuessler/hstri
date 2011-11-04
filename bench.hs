{-# LANGUAGE FlexibleInstances #-}

import Triangulation
import TriangulationCxtObject
import Criterion.Main
import Test.QuickCheck
import System.Random
import FaceLattice hiding(main)
import Data.GraphViz
import Test.QuickCheck.Gen
import Control.DeepSeq
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Data.Colour.SRGB as SRGB


fixedStdGen = mkStdGen 238987121

qcArgs sz = stdArgs { replay = Just (fixedStdGen,sz), maxSuccess = 20 }

bench1 = quickCheckWith (qcArgs 50) prop_IsSubface_VE_welldefined



bench2 = nf (`fmap` [1..20]) go
    where
        go seed = 
                let t = unGen arbitrary (mkStdGen seed) 260 :: Triangulation
                    fl = faceLattice t
                in fl


main = defaultMain [
            --bench "VE subface welldef (size 50)" bench1 
            bench "Dotgraphify face lattice" bench2 

        ]

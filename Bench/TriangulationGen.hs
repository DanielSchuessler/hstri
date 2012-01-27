module Bench.TriangulationGen where

import Triangulation.Random
import Language.Haskell.TH.Lift
import System.Random
import Test.QuickCheck.Gen
import Test.QuickCheck
import Language.Haskell.TH


trsGen = let
            gen = vectorOf 10 
                    (let n = 20
                         g = 26
                            
                        in randomManifoldT n g g)

        in do
            report False "Generating triangulations..."  
            lift (unGen gen (mkStdGen 1) 20)

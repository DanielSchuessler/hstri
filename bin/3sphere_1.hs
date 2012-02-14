{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, GADTs, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving #-} 

import HsTri

import Data.Function

tr = snd tr_0

spqwc = oneTetWithDefaultCoords tr f
    where
        f g | ngDomOrCodIs (0 ./ tABC) g = "F"
            | ngDomOrCodIs (0 ./ tACD) g = "G"


standardToQuadResults = quadToStandardSolSet tr (qVertexSolutions tr) 


main = do
    pr (ppSSCRes standardToQuadResults)

goblender = do
    testBlender . setCam oneTetCam . defaultScene . fromSpqwc $ spqwc 

cutq_before = readRgaFile "3sphere_cutq_before.xml" 

-- main = 

{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, GADTs, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving #-} 

import HsTri

import Data.Function

tr = snd tr_0

spqwc = oneTetWithDefaultCoords tr f
    where
        f g | ngDomOrCodIs (0 ./ tABC) g = "F"
            | ngDomOrCodIs (0 ./ tACD) g = "G"

goblender = do
    testBlender . setCam oneTetCam . defaultScene . fromSpqwc $ spqwc 

-- main = 

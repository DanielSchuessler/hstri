{-# LANGUAGE TemplateHaskell #-}
module AbstractTetrahedron2 where

import S3
import Triangle
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List((\\))
import Util

$( let
        vs = [vA,vB,vC,vD]
        f v0 v1 v2 = valD (varP n) (normalB (lift y)) []
            where
                n = mkName ("o"++show y)
                y = otriangle (v0,v1,v2)
   in
    sequence [ f v0 v1 v2 | v0 <- vs, v1 <- vs \\ [v0], v2 <- vs \\ [v0,v1] ])

instance Quote OTriangle where
    quotePrec _ ot = "o"++show ot

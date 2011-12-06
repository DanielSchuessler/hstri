{-# LANGUAGE TemplateHaskell #-}
module AbstractTetrahedron2 where

import S3
import Triangle
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List((\\))
import Util
import Quote
import THUtil

mkConstantDecls [minBound .. maxBound :: OTriangle] lift

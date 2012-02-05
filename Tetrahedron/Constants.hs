{-# LANGUAGE TemplateHaskell #-}
module Tetrahedron.Constants where

import Math.Groups.S3
import Tetrahedron.Triangle
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List((\\))
import Util
import Quote
import THUtil

mkConstantDecls [minBound .. maxBound :: OEdge] lift
mkConstantDecls [minBound .. maxBound :: OTriangle] lift


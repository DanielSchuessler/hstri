{-# LANGUAGE TemplateHaskell #-}
module Tetrahedron.NormalConstants where

import THUtil
import Tetrahedron.NormalDisc
import Language.Haskell.TH.Syntax

mkConstantDecls allNormalCorners lift
mkConstantDecls allNormalArcs lift
mkConstantDecls allNormalTris lift

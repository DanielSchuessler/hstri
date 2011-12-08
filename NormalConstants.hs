{-# LANGUAGE TemplateHaskell #-}
module NormalConstants where

import THUtil
import NormalDisc
import Language.Haskell.TH.Syntax

mkConstantDecls allNormalCorners lift
mkConstantDecls allNormalArcs lift
mkConstantDecls allNormalTris lift

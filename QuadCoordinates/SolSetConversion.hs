{-# LANGUAGE FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module QuadToStandard.SolSetConversion where

import qualified Data.Vector as V
import QuadCoordinates.CanonExt
import PrettyUtil
import Data.BitVector.Adaptive
import Triangulation


quadToStandardSolSet tr quadSolSet =
    let
        _L_0 = V.map canonExt quadSolSet 
        _C_0 = bvEmpty (3 * tNumberOfTetrahedra_ tr) 

        vertexStep (_L_prev,_C_prev) =

        
    in 
        foldr vertexStep (_L_0, _C_0) 

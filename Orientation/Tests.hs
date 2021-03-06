{-# LANGUAGE TemplateHaskell #-}
module Orientation.Tests where

import ClosedOrCensus6
import ClosedNorCensus8
import Orientation
import Util
import Test.QuickCheck.All
import QuickCheckUtil
import Triangulation.Class
import Test.QuickCheck
import TriangulationCxtObject
import qualified Data.Vector.Generic as VG
import Data.SumType

prop_closedOrCensus6_oriented :: Property
prop_closedOrCensus6_oriented =
    forAllElements closedOrCensus6
        (\ltr ->
            let
                tr = toTriangulation ltr 
                r = orientTriangulation tr 
            in
                case r of
                     Left r' ->
                         printTestCase (show r') False

                     Right o ->
                         VG.length o .=. tNumberOfTetrahedra tr)

prop_closedNorCensus8_not_oriented :: Property
prop_closedNorCensus8_not_oriented =
    forAllElements closedNorCensus8
        (\ltr ->
            let
                tr = toTriangulation ltr 
                r = orientTriangulation tr 
            in
                printTestCase (show r) $
--                 collect r $
                isLeft r)

prop_inducedOrient32_equivariant t = 
    let
        f = flip inducedOrient32 t
    in
        \ori -> f (flipSo ori) .=. flipSo (f ori)

prop_inducedOrient21_equivariant t e = 
        isSubface e t ==>
    let
        f = \ori -> inducedOrient21 ori t e
    in
        \ori -> f (flipSo ori) .=. flipSo (f ori)

prop_inducedOrient10_equivariant e v = 
        isSubface v e ==>
    let
        f = \ori -> inducedOrient10 ori e v
    in
        \ori -> f (flipSo ori) .=. flipSo (f ori)

qc_Orientation = $quickCheckAll



{-# LANGUAGE ViewPatterns, ScopedTypeVariables #-}
module Codec.Rga.Util where

import TIndex
import HomogenousTuples
import Data.Tuple.Index

type S4 = Quadruple Index4

s4fromInt :: Int -> S4
s4fromInt (dcba :: Int) =
    let 
                        divMod4 = (`divMod` 4)
                        (dcb,a) = divMod4 dcba
                        (dc,b) = divMod4 dcb
                        (d,c) = divMod4 dc
    in map4 toEnum (a,b,c,d)

s4toInt :: S4 -> Int
s4toInt (map4 fromEnum -> (a::Int,b,c,d)) = ((((d * 4 + c) * 4) + b) * 4) + a


type RgaGluingsForTet = Quadruple (Maybe (TIndex,S4))


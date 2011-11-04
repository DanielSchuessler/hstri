{-# OPTIONS -Wall #-}
module Indexing where

import qualified Data.Map as M

newtype Indexing a = Indexing {
    indexing_map :: M.Map a Int
    -- Invariant: snd (elemAt i indexing_map) == i
}
    deriving (Show)

fromList :: Ord a => [a] -> Indexing a
fromList xs = Indexing (M.fromList (zip xs [0..]))

toList :: Indexing k -> [k]
toList = M.keys . indexing_map

toInt :: Ord k => Indexing k -> k -> Int
toInt t x = indexing_map t M.! x

fromInt :: Indexing a -> Int -> a
fromInt t i = fst (M.elemAt i (indexing_map t))


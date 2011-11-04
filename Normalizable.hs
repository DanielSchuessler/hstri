module Normalizable where

class Normalizable a where
    normalize :: a -> a

isNormalized ::  (Normalizable a, Eq a) => a -> Bool
isNormalized x = x == normalize x


module DependentVector where

import AnySimplex
import Data.Vector as V

newtype DependentVector a = Vector (AnySimplex a)
    -- INVARIANT: The i-th element contains an i-dimensional item




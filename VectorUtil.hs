{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module VectorUtil where
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import QuickCheckUtil
import Data.Maybe
import Test.QuickCheck


v_mapMaybe
  :: (VG.Vector v a, VG.Vector v1 a1) =>
     (a1 -> Maybe a) -> v1 a1 -> v a
v_mapMaybe f xs = VG.unfoldrN (VG.length xs) u 0
    where
        u i = case xs VG.!? i of
                   Nothing -> Nothing
                   Just x -> case f x of
                                  Nothing -> u (i+1)
                                  Just y -> Just (y,i+1)

prop_mapMaybe :: Blind (Int -> Maybe Bool) -> [Int] -> Property
prop_mapMaybe (Blind (f :: Int -> Maybe Bool)) xs =
    mapMaybe f xs .=. (V.toList . v_mapMaybe f . V.fromList) xs 

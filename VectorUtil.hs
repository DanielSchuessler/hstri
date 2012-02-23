{-# LANGUAGE TypeFamilies, RecordWildCards, TemplateHaskell, NamedFieldPuns, FlexibleContexts, TupleSections, NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module VectorUtil where
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import QuickCheckUtil
import Data.Maybe
import Test.QuickCheck
import TupleTH
import PrettyUtil.Matrix
import HomogenousTuples
import Element


{-# INLINABLE vectorMapMaybe #-}
vectorMapMaybe
  :: (VG.Vector v (Maybe b), VG.Vector v b, VG.Vector v a) =>
     (a -> Maybe b) -> v a -> v b
vectorMapMaybe f = VG.map fromJust . VG.filter isJust . VG.map f

prop_mapMaybe :: Blind (Int -> Maybe Bool) -> [Int] -> Property
prop_mapMaybe (Blind (f :: Int -> Maybe Bool)) xs =
    mapMaybe f xs .=. (V.toList . vectorMapMaybe f . V.fromList) xs 

data PartitioningBySign v r = PartitioningBySign {   
    _Sneg, _S0, _Spos :: v r
}
    deriving Show

type instance Element (PartitioningBySign v r) = Element (v r) 

instance AsList (v r) => AsList (PartitioningBySign v r) where
    asList (PartitioningBySign a b c) = asList a ++ asList b ++ asList c

instance MaxColumnWidth (v r) => MaxColumnWidth (PartitioningBySign v r) where
    maxColumnWidth PartitioningBySign{..} =

         $(foldr1Tuple 3) max (map3 maxColumnWidth (_Sneg,_S0,_Spos))



{-# INLINABLE partitionBySign #-}
partitionBySign
  :: (Num b, Ord b) =>
     (r -> b) -> V.Vector r -> PartitioningBySign V.Vector r
partitionBySign f _Vp = PartitioningBySign {_Sneg,_S0,_Spos}
    where
                (_Sneg,_Snn ) = V.partition ((<0)  . f) _Vp
                (_S0  ,_Spos) = V.partition ((==0) . f) _Snn


{-# INLINABLE vectorCart #-}
vectorCart :: (VG.Vector v t, VG.Vector v a, VG.Vector v (t, a)) => v t -> v a -> v (t, a)
vectorCart v w = 
    let
        m = VG.length w
    in
        VG.generate (VG.length v * m)
            (\i -> case divMod i m of (j,k) -> (VG.unsafeIndex v j, VG.unsafeIndex w k)) 

vectorAdjustAt :: VG.Vector v a => v a -> (a -> a) -> Int -> v a
vectorAdjustAt v f i = v VG.// [(i,f (v VG.! i))]

vectorAdjustAtF
  :: (Functor f, VG.Vector v a) =>
     v a -> (a -> f a) -> Int -> f (v a)
vectorAdjustAtF v f i =
    fmap (\y -> v VG.// [(i,y)]) (f (v VG.! i))

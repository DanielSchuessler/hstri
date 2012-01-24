{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Math.Groups.Tests where

import Math.Group
import Math.Groups.S3
import Data.Proxy
import Test.QuickCheck
import Test.QuickCheck.All
import QuickCheckUtil
import Data.Tuple.Index
import Control.Monad

-- * S2
prop_s2group = polyprop_group (undefined :: Proxy S2)

-- * S3
prop_s3group = polyprop_group (undefined :: Proxy S3)

prop_tupleFromToFun :: (Int, Int, Int) -> Property
prop_tupleFromToFun (x :: (Int,Int,Int)) = x .=. tupleFromFun3 (tupleToFun3 x) 

prop_toFun_homomorphism :: S3 -> S3 -> Index3 -> Property
prop_toFun_homomorphism g2 g1 x = 
    s3ToFun g2 (s3ToFun g1 x) .=. s3ToFun (g2 .*. g1) x

prop_fromFun_homomorphism
  :: (Index3 -> Index3) -> (Index3 -> Index3) -> Property
prop_fromFun_homomorphism f2 f1 =
    s3FromFunMay (f2 . f1) .=. liftM2 (.*.) (s3FromFunMay f2) (s3FromFunMay f1)

prop_act_id :: (Int,Int,Int) -> Property
prop_act_id = polyprop_ract_id (undefined :: Proxy S3)

prop_permute_mult :: S3 -> S3 -> (Int,Int,Int) -> Property
prop_permute_mult = polyprop_ract_mult

prop_sort3WithPermutation :: (Int, Int, Int) -> Property
prop_sort3WithPermutation (xs :: (Int,Int,Int)) = 
    case sort3WithPermutation xs of
         (xs'@(x0,x1,x2),g) -> 
         
            x0 <= x1 .&. 
            x1 <= x2 .&.
            xs .=. xs' *. g

prop_s3sgn :: Property
prop_s3sgn = isGroupHomo s3sgn

qc_Math_Groups_Tests = $quickCheckAll

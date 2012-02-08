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


prop_Pair_S2_RightAction :: Property
prop_Pair_S2_RightAction = polyprop_ract (undefined :: Proxy ((Int,Int),S2))

prop_Triple_S3_RightAction :: Property
prop_Triple_S3_RightAction = polyprop_ract (undefined :: Proxy ((Int,Int,Int),S3))


prop_sort3WithPermutation :: (Int, Int, Int) -> Property
prop_sort3WithPermutation (xs :: (Int,Int,Int)) = 
    case sort3WithPermutation xs of
         (xs'@(x0,x1,x2),g) -> 
         
            x0 <= x1 .&. 
            x1 <= x2 .&.
            xs .=. xs' *. g

prop_s3sgn :: Property
prop_s3sgn = isGroupHomo s3sgn

prop_S2_Index2_LeftAction :: Property
prop_S2_Index2_LeftAction = polyprop_lact (undefined :: Proxy (S2,Index2))

prop_S3_Index3_LeftAction :: Property
prop_S3_Index3_LeftAction = polyprop_lact (undefined :: Proxy (S3,Index3))

qc_Math_Groups_Tests = $quickCheckAll

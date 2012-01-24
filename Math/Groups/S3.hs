{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module Math.Groups.S3(module Math.Group, module Math.Groups.S2, S3(..),allS3,sort3WithPermutation,
    transpositions,
    s3ToFun,
    s3FromFunMay,
    s3sgn,

    -- * Testing
    qc_S3
    ) where

import Data.Binary
import Data.Hashable
import Data.Maybe
import Data.Monoid
import Data.Tuple.Index
import HomogenousTuples
import Language.Haskell.TH.Syntax
import Math.Group
import Math.Groups.S2
import PrettyUtil
import Quote
import THUtil
import Test.QuickCheck
import Test.QuickCheck.All
import Util

-- | Symmetric group / permutation group on 3 elements
data S3 = S3abc | S3bca | S3cab | S3acb | S3bac | S3cba deriving(Read,Show,Eq,Enum,Bounded,Ord)

-- toPermute :: S3 -> Permute
-- toPermute = charsToPermute 3 . drop 2 . show 
-- 
-- fromPermute :: Permute -> S3
-- fromPermute = read . ("S3"++) . permuteToChars
--
-- prop_fromToPermute :: S3 -> Property 
-- prop_fromToPermute x = x .=. fromPermute (toPermute x)


allS3 :: [S3]
allS3 = [minBound .. maxBound]

instance Arbitrary S3 where 
    arbitrary = elements allS3 
    shrink = monoidDefaultShrink

instance CoArbitrary S3 where coarbitrary = variant . fromEnum

instance Pretty S3 where pretty = yellow . text . drop 2 . show 

-- | Returns an element of S3 satisfying the given predicate
s3find ::  (S3 -> Bool) -> Maybe S3
s3find p 
    | p S3abc = Just S3abc
    | p S3acb = Just S3acb
    | p S3bac = Just S3bac
    | p S3bca = Just S3bca
    | p S3cba = Just S3cba
    | p S3cab = Just S3cab
    | otherwise = Nothing

s3the ::  (S3 -> Bool) -> S3
s3the = fromMaybe err . s3find
    where
        err = error "s3the: no element satisfies the predicate"




s3ToFun ::  S3 -> Index3 -> Index3
s3ToFun g = case g of
                       S3abc -> mkFun I3_0 I3_1 I3_2
                       S3bca -> mkFun I3_1 I3_2 I3_0
                       S3cab -> mkFun I3_2 I3_0 I3_1
                       S3acb -> mkFun I3_0 I3_2 I3_1
                       S3bac -> mkFun I3_1 I3_0 I3_2
                       S3cba -> mkFun I3_2 I3_1 I3_0
    where
        mkFun imA _ _ I3_0 = imA
        mkFun _ imB _ I3_1 = imB
        mkFun _ _ imC _ = imC

s3FromFunMay ::  (Index3 -> Index3) -> Maybe S3
s3FromFunMay f = case tupleFromFun3 f of
                 (I3_0,I3_1,I3_2) -> Just S3abc
                 (I3_0,I3_2,I3_1) -> Just S3acb
                 (I3_1,I3_0,I3_2) -> Just S3bac
                 (I3_1,I3_2,I3_0) -> Just S3bca
                 (I3_2,I3_0,I3_1) -> Just S3cab
                 (I3_2,I3_1,I3_0) -> Just S3cba
                 _ -> Nothing

fromBijFun :: (Index3 -> Index3) -> S3
fromBijFun f = case (f I3_0, f I3_1) of
                 (I3_0,I3_1) -> S3abc
                 (I3_0,I3_2) -> S3acb
                 (I3_1,I3_0) -> S3bac
                 (I3_1,I3_2) -> S3bca
                 (I3_2,I3_0) -> S3cab
                 (I3_2,I3_1) -> S3cba
                 _ -> error "fromBijFun: not bijective"
                








s3mult' :: S3 -> S3 -> S3
s3mult' S3abc S3abc = S3abc
s3mult' S3bca S3abc = S3bca
s3mult' S3cab S3abc = S3cab
s3mult' S3acb S3abc = S3acb
s3mult' S3bac S3abc = S3bac
s3mult' S3cba S3abc = S3cba
s3mult' S3abc S3bca = S3bca
s3mult' S3bca S3bca = S3cab
s3mult' S3cab S3bca = S3abc
s3mult' S3acb S3bca = S3cba
s3mult' S3bac S3bca = S3acb
s3mult' S3cba S3bca = S3bac
s3mult' S3abc S3cab = S3cab
s3mult' S3bca S3cab = S3abc
s3mult' S3cab S3cab = S3bca
s3mult' S3acb S3cab = S3bac
s3mult' S3bac S3cab = S3cba
s3mult' S3cba S3cab = S3acb
s3mult' S3abc S3acb = S3acb
s3mult' S3bca S3acb = S3bac
s3mult' S3cab S3acb = S3cba
s3mult' S3acb S3acb = S3abc
s3mult' S3bac S3acb = S3bca
s3mult' S3cba S3acb = S3cab
s3mult' S3abc S3bac = S3bac
s3mult' S3bca S3bac = S3cba
s3mult' S3cab S3bac = S3acb
s3mult' S3acb S3bac = S3cab
s3mult' S3bac S3bac = S3abc
s3mult' S3cba S3bac = S3bca
s3mult' S3abc S3cba = S3cba
s3mult' S3bca S3cba = S3acb
s3mult' S3cab S3cba = S3bac
s3mult' S3acb S3cba = S3bca
s3mult' S3bac S3cba = S3cab
s3mult' S3cba S3cba = S3abc

instance Semigroup S3 where
    (<>) = s3mult'

instance Monoid S3 where
    mappend = (<>)
    mempty = S3abc

instance Group S3 where
    inv g = s3the (\g' -> g' .*. g == S3abc)

instance RightAction S3 (a,a,a) where
    xs *. g = tupleFromFun3 . (. s3ToFun g) . tupleToFun3 $ xs 



-- | Tests
qc_S3 ::  IO Bool
qc_S3 = $(quickCheckAll)


instance Hashable S3 where hash = fromEnum
instance Finite S3

instance Lift S3 where
    lift = liftByShow


instance Quote S3 where
    quotePrec _ = show


-- | 
-- Property: @xs == uncurry ('*.') (sort3WithPermutation xs)@ 
--
-- Property: @fst (sort3WithPermutation)@ is sorted
sort3WithPermutation :: Ord t => (t, t, t) -> ((t, t, t),S3)
sort3WithPermutation xs@(x0,x1,x2) =
    if x0<=x1
       then 
        if x1<=x2
           then (xs,mempty)
           else -- x2 < x1 
            if x0<=x2
               then ((x0,x2,x1),S3acb)
               else -- x2 < x0 
                ((x2,x0,x1),S3bca)
       else -- x1 < x0
        if x0<=x2
           then ((x1,x0,x2),S3bac)
           else -- x2 < x0
            if x1<=x2
               then ((x1,x2,x0),S3cab)
               else -- x2 < x1
                ((x2,x1,x0),S3cba)



instance Binary S3 where
    get = getEnumWord8
    put = putEnumWord8

transpositions :: (S3, S3, S3)
transpositions = (S3bac, S3cba, S3acb)

-- | Signum
s3sgn :: S3 -> S2
s3sgn g = if g `elem3` transpositions
             then Flip
             else NoFlip

instance Signum S3 where sgn = s3sgn


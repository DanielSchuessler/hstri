{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module S3(module Group, S3(..),allS3,sort3WithPermutation,

    -- * Testing
    qc_S3
    ) where

import Data.Hashable
import Data.Monoid
import Language.Haskell.TH.Syntax
import Test.QuickCheck
import Test.QuickCheck.All
import PrettyUtil
import Util
import Group
import THUtil
import Quote
import Data.Maybe
import QuickCheckUtil
import Data.Proxy
import Control.Monad

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

instance Arbitrary S3 where arbitrary = elements allS3 
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

data ABC = A | B | C deriving(Eq,Show)

instance Arbitrary ABC where arbitrary = elements [A,B,C]
instance CoArbitrary ABC where coarbitrary = coarbitraryShow
instance Show (ABC -> ABC) where showsPrec prec f = showParen (prec > 10) (showString "tupleToFun " . shows (tupleFromFun f))                                

tupleToFun :: (t, t, t) -> ABC -> t
tupleToFun (a,_,_) A = a
tupleToFun (_,b,_) B = b
tupleToFun (_,_,c) C = c

tupleFromFun :: (ABC -> t) -> (t, t, t)
tupleFromFun f = (f A, f B, f C)

prop_tupleFromToFun :: (Int, Int, Int) -> Property
prop_tupleFromToFun (x :: (Int,Int,Int)) = x .=. tupleFromFun (tupleToFun x) 


toFun ::  S3 -> ABC -> ABC
toFun g = case g of
                       S3abc -> mkFun A B C
                       S3bca -> mkFun B C A
                       S3cab -> mkFun C A B
                       S3acb -> mkFun A C B
                       S3bac -> mkFun B A C
                       S3cba -> mkFun C B A
    where
        mkFun imA _ _ A = imA
        mkFun _ imB _ B = imB
        mkFun _ _ imC _ = imC

fromFunMay ::  (ABC -> ABC) -> Maybe S3
fromFunMay f = case tupleFromFun f of
                 (A,B,C) -> Just S3abc
                 (A,C,B) -> Just S3acb
                 (B,A,C) -> Just S3bac
                 (B,C,A) -> Just S3bca
                 (C,A,B) -> Just S3cab
                 (C,B,A) -> Just S3cba
                 _ -> Nothing

fromBijFun :: (ABC -> ABC) -> S3
fromBijFun f = case (f A, f B) of
                 (A,B) -> S3abc
                 (A,C) -> S3acb
                 (B,A) -> S3bac
                 (B,C) -> S3bca
                 (C,A) -> S3cab
                 (C,B) -> S3cba
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


prop_toFun_homomorphism :: S3 -> S3 -> ABC -> Property
prop_toFun_homomorphism g2 g1 x = 
    toFun g2 (toFun g1 x) .=. toFun (g2 .*. g1) x

prop_fromFun_homomorphism
  :: (ABC -> ABC) -> (ABC -> ABC) -> Property
prop_fromFun_homomorphism f2 f1 =
    fromFunMay (f2 . f1) .=. liftM2 (.*.) (fromFunMay f2) (fromFunMay f1)

instance Monoid S3 where
    mappend = s3mult'
    mempty = S3abc

prop_idl :: S3 -> Bool
prop_idl = polyprop_idl

prop_idr :: S3 -> Bool
prop_idr = polyprop_idr

prop_assoc :: S3 -> S3 -> S3 -> Bool
prop_assoc = polyprop_assoc


instance Group S3 where
    inv g = s3the (\g' -> g' .*. g == S3abc)

prop_invl :: S3 -> Bool
prop_invl = polyprop_invl 

prop_invr :: S3 -> Bool
prop_invr = polyprop_invr 


instance RightAction S3 (a,a,a) where
    xs *. g = tupleFromFun . (. toFun g) . tupleToFun $ xs 


prop_act_id :: (Int,Int,Int) -> Bool
prop_act_id = polyprop_ract_id (undefined :: Proxy S3)

prop_permute_mult :: S3 -> S3 -> (Int,Int,Int) -> Bool
prop_permute_mult = polyprop_ract_mult


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


prop_sort3WithPermutation :: (Int, Int, Int) -> Property
prop_sort3WithPermutation (xs :: (Int,Int,Int)) = 
    case sort3WithPermutation xs of
         (xs'@(x0,x1,x2),g) -> 
         
            x0 <= x1 .&. 
            x1 <= x2 .&.
            xs .=. xs' *. g


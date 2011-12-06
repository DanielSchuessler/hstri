{-# LANGUAGE TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module S3(module Group, S3(..),allS3,qc_S3) where

import Collections
import Data.Hashable
import Data.Monoid
import Data.Tagged
import Language.Haskell.TH.Syntax
import Test.QuickCheck
import Test.QuickCheck.All
import Text.PrettyPrint.ANSI.Leijen
import Util
import Group
import THUtil
import Quote

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

instance Pretty S3 where pretty = yellow . text . drop 2 . show 

-- | Returns an element of S3 satisfying the given predicate
s3the ::  (S3 -> Bool) -> S3
s3the p 
    | p S3abc = S3abc
    | p S3acb = S3acb
    | p S3bac = S3bac
    | p S3bca = S3bca
    | p S3cba = S3cba
    | p S3cab = S3cab
    | otherwise = error "s3the: no element satisfies the predicate"

data ABC = A | B | C deriving(Eq)

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


fromFun ::  (ABC -> ABC) -> S3
fromFun f = s3the (\g ->  
                          toFun g A == f A 
                       && toFun g B == f B)


instance Monoid S3 where
    mappend g2 g1 = fromFun (toFun g1 . toFun g2)
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


instance LeftAction S3 (a,a,a) where
    g .* x@(a,b,c) = case g of
                       S3abc -> x
                       S3bca -> (b,c,a)
                       S3cab -> (c,a,b)
                       S3acb -> (a,c,b)
                       S3bac -> (b,a,c)
                       _ {- S3cba -} -> (c,b,a)



prop_act_id :: (Int,Int,Int) -> Bool
prop_act_id = untag (polyprop_act_id :: Tagged S3 ((Int,Int,Int) -> Bool))

prop_permute_mult :: S3 -> S3 -> (Int,Int,Int) -> Bool
prop_permute_mult = polyprop_act_mult


-- | Tests
qc_S3 ::  IO Bool
qc_S3 = $(quickCheckAll)

deriveCollectionKeyClass ''S3

instance Hashable S3 where hash = fromEnum
instance Finite S3

instance Lift S3 where
    lift = liftByShow


instance Quote S3 where
    quotePrec _ = show

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, TemplateHaskell, CPP #-}
{-# OPTIONS -Wall #-}
module S2 where

import Collections
import Data.Hashable
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Test.QuickCheck
import PrettyUtil
import Group
import Util
import Data.Binary


data S2 = NoFlip | Flip deriving(Show,Enum,Bounded,Eq,Ord)

instance Monoid S2 where
    mempty = NoFlip
    NoFlip `mappend` x = x
    x `mappend` NoFlip = x
    _ `mappend` _ = NoFlip

instance Group S2 where
    inv _ = Flip

instance RightAction S2 (a,a) where
    (x,y) *. NoFlip = (x,y)
    (x,y) *. Flip = (y,x)

instance Pretty S2 where
    pretty = yellow . text . show

instance Arbitrary S2 where arbitrary = elements [NoFlip,Flip]
instance CoArbitrary S2 where coarbitrary = variant . fromEnum


deriveCollectionKeyClass ''S2

instance Hashable S2 where hash = fromEnum

instance Finite S2

instance Lift S2 where
    lift x = conE (mkName (show x))

sort2WithPermutation :: Ord t => (t, t) -> ((t, t), S2)
sort2WithPermutation (x,y) =
    if (x<=y) 
       then ((x,y),NoFlip)
       else ((y,x),Flip)

instance Binary S2 where
    get = getEnumWord8
    put = putEnumWord8


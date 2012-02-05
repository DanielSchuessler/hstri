{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, TemplateHaskell, CPP #-}
{-# OPTIONS -Wall #-}
module Math.Groups.S2 where

import Data.Binary
import Data.Hashable
import Data.Monoid
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Math.Group
import PrettyUtil
import Test.QuickCheck
import Util


data S2 = NoFlip | Flip deriving(Show,Enum,Bounded,Eq,Ord)

instance Semigroup S2 where
    NoFlip <> x = x
    Flip <> NoFlip = Flip
    Flip <> Flip = NoFlip

instance Monoid S2 where
    mempty = NoFlip
    mappend = (<>)

instance Group S2 where
    inv = id

instance RightAction S2 (a,a) where
    (x,y) *. NoFlip = (x,y)
    (x,y) *. Flip = (y,x)

instance Pretty S2 where
    pretty = yellow . text . show

instance Arbitrary S2 where 
    arbitrary = elements [NoFlip,Flip]
    shrink = monoidDefaultShrink

instance CoArbitrary S2 where coarbitrary = variant . fromEnum



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

class Signum a where
    sgn :: a -> S2

instance Signum S2 where sgn = id


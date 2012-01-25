{-# LANGUAGE DeriveFunctor,DeriveFoldable,DeriveTraversable, NoMonomorphismRestriction #-}
module Data.FormalOps where

import Data.Foldable(Foldable)
import Data.Traversable(Traversable)
import Control.Applicative
import Data.Monoid

data FormalProduct a b = a :* b
    deriving (Show,Functor,Foldable,Traversable,Eq,Ord)

infixl 7 :*

data FormalSum a b = a :+ b
    deriving (Show,Eq,Ord)

infixl 6 :+

bimapFormalSum
  :: (t -> a) -> (t1 -> b) -> FormalSum t t1 -> FormalSum a b
bimapFormalSum fa fb (a :+ b) = fa a :+ fb b

bifoldMapFormalSum
  :: Monoid a => (t -> a) -> (t1 -> a) -> FormalSum t t1 -> a
bifoldMapFormalSum fa fb (a :+ b) = mappend (fa a) (fb b)

bitraverseFormalSum
  :: Applicative f =>
     (a -> f a') -> (b -> f b') -> FormalSum a b -> f (FormalSum a' b')
bitraverseFormalSum fa fb (a :+ b) = (:+) <$> fa a <*> fb b

foldFormalSum :: (a -> b -> r) -> FormalSum a b -> r
foldFormalSum f ((:+) a b) = f a b

evalFormalSum :: Num r => FormalSum r r -> r
evalFormalSum = foldFormalSum (+)


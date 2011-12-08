{-# LANGUAGE Rank2Types, ExistentialQuantification, TypeOperators, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module Simplicial.AnySimplex(
    AnySimplex(..),anySimplex_dim,
    elimAnySimplexWithNat, foldAnySimplexWithNat,
    simpleMapAnySimplex,
    module TypeLevel.TF.Nat,
    module TypeLevel.TF.Fun,
    module TypeLevel.TF.Nat.Small,

    ShowN(..),showN,showsPrecN,
    OrdN(..),compareN

 ) where

import TypeLevel.TF.Nat
import TypeLevel.TF.Fun
import TypeLevel.TF.Nat.Small
import Data.Proxy
import PrettyUtil
import Either1

data AnySimplex a = forall n. Nat n => AnySimplex (a n)

elimAnySimplexWithNat :: AnySimplex a -> (forall n. Nat n => n -> a n -> r) -> r
elimAnySimplexWithNat (AnySimplex x) f = f undefined x

foldAnySimplexWithNat
  :: (forall n. Nat n => n -> a n -> c) -> AnySimplex a -> c
foldAnySimplexWithNat = flip elimAnySimplexWithNat

anySimplex_dim :: AnySimplex a -> Int
anySimplex_dim a = elimAnySimplexWithNat a (const . natToInt)


simpleMapAnySimplex :: (forall n. n -> a n -> b n) -> AnySimplex a -> AnySimplex b
simpleMapAnySimplex f (AnySimplex a) = AnySimplex (f undefined a)



instance ShowN a => Show (AnySimplex a) where
    showsPrec prec (AnySimplex x) =
        showsPrecN prec x


instance ShowN a => Pretty (AnySimplex a) where
    pretty = string . show

instance OrdN a => Eq (AnySimplex a) where
    a == b = compare a b == EQ

compareAnySimplex :: OrdN t => AnySimplex t -> AnySimplex t -> Ordering
compareAnySimplex (AnySimplex (x1 :: a n1)) (AnySimplex (x2 :: a n2)) =
        caseEqNat n1_ n2_ 
             (compareN x1 x2)
             (compare (natToInt n1_) (natToInt n2_))
      where
        n1_ = undefined :: n1
        n2_ = undefined :: n2    

instance OrdN a => Ord (AnySimplex a) where
    compare = compareAnySimplex 

-- Stuff that doesn't really belong in this module
-- | Means that @a n@ is 'Show' for all @'Nat' n@
class ShowN a where
    getShow :: Nat n => Proxy (a n) -> (Show (a n) => r) -> r

-- | Means that @a n@ is 'Ord for all @'Nat' n@
class OrdN a where
    getOrd :: Nat n => Proxy (a n) -> (Ord (a n) => r) -> r

showsPrecN :: forall a n. (ShowN a, Nat n) => Int -> a n -> ShowS
showsPrecN prec x = getShow (prox x) (showsPrec prec x)

showN :: (Nat n, ShowN a) => a n -> String
showN x = showsPrecN 0 x ""

compareN :: (OrdN a) => Nat n => a n -> a n -> Ordering
compareN x y = getOrd (prox x) (compare x y) 



prox :: a -> Proxy a
prox _ = undefined

instance (ShowN s1, ShowN s2) => ShowN (Either1 s1 s2) where
    getShow (_ :: Proxy (Either1 s1 s2 n)) r = 
        getShow (prox (undefined :: s1 n))
            (getShow (prox (undefined :: s2 n))
                r)

instance (OrdN s1, OrdN s2) => OrdN (Either1 s1 s2) where
    getOrd (_ :: Proxy (Either1 s1 s2 n)) r = 
        getOrd (prox (undefined :: s1 n))
            (getOrd (prox (undefined :: s2 n))
                r)


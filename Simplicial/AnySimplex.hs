{-# LANGUAGE Rank2Types, ExistentialQuantification, TypeOperators, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module Simplicial.AnySimplex(
    AnySimplex(..),anySimplex_dim,elimAnySimplexWithNat,simpleMapAnySimplex,
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

data AnySimplex a = forall n. Nat n => AnySimplex (a n)

elimAnySimplexWithNat :: AnySimplex a -> (forall n. Nat n => n -> a n -> r) -> r
elimAnySimplexWithNat (AnySimplex x) f = f undefined x

anySimplex_dim :: AnySimplex a -> Int
anySimplex_dim (AnySimplex (_ :: a n)) = natToInt (undefined :: n)

simpleMapAnySimplex :: (forall n. n -> a n -> b n) -> AnySimplex a -> AnySimplex b
simpleMapAnySimplex f (AnySimplex a) = AnySimplex (f undefined a)



-- Stuff that doesn't really belong in this module
instance ShowN a => Show (AnySimplex a) where
    showsPrec prec (AnySimplex x) =
        showsPrecN prec x

instance OrdN a => Eq (AnySimplex a) where
    a == b = compare a b == EQ


instance OrdN a => Ord (AnySimplex a) where
    compare (AnySimplex (x1 :: a n1)) (AnySimplex (x2 :: a n2)) =
        caseEqNat n1_ n2_ 
             (compareN x1 x2)
             (compare (natToInt n1_) (natToInt n2_))
      where
        n1_ = undefined :: n1
        n2_ = undefined :: n2

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


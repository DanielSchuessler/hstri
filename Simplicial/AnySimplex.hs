{-# LANGUAGE ExistentialQuantification, TypeOperators, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module Simplicial.AnySimplex(
    AnySimplex(..),anySimplex_dim,
    module TypeLevel.TF

 ) where
import TypeLevel.TF

data AnySimplex a = forall n. Nat n => AnySimplex n (a :$ n)

anySimplex_dim :: AnySimplex t -> Int
anySimplex_dim (AnySimplex n _) = natToInt n

instance ShowN a => Show (AnySimplex a) where
    showsPrec prec (AnySimplex n x) =
        showsPrecN (undefined :: a) n prec x

instance OrdN a => Eq (AnySimplex a) where
    a == b = compare a b == EQ


instance OrdN a => Ord (AnySimplex a) where
    compare (AnySimplex n1_ x1) (AnySimplex n2_ x2) =
        caseEqNat n1_ n2_ 
             (getOrd (undefined :: a) n1_ (compare x1 x2))
             (compare (natToInt n1_) (natToInt n2_))



{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Group where
import Data.Monoid
import Data.Tagged
import qualified Data.Vect.Double as V

(.*.) ::  Monoid a => a -> a -> a
(.*.) = mappend
infixr 7 .*.

polyprop_idl ::  (Monoid a, Eq a) => a -> Bool
polyprop_idl g = mempty .*. g == g

polyprop_idr ::  (Monoid a, Eq a) => a -> Bool
polyprop_idr g = g .*. mempty == g

polyprop_assoc ::  (Monoid a, Eq a) => a -> a -> a -> Bool
polyprop_assoc g3 g2 g1 = (g3 .*. g2) .*. g1 == g3 .*. (g2 .*. g1)


class Monoid g => Group g where
    inv :: g -> g


polyprop_invl ::  (Group a, Eq a) => a -> Bool
polyprop_invl g = inv g .*. g == mempty

polyprop_invr ::  (Group a, Eq a) => a -> Bool
polyprop_invr g = g .*. inv g == mempty

class LeftAction g x where
    (.*) ::  g -> x -> x

infixr 7 .*

-- We need a type-level tag here because the monoid isn't inferrable from the acted-upon type
polyprop_act_id :: forall a x. (Monoid a, LeftAction a x, Eq x) => Tagged a (x -> Bool)
polyprop_act_id = Tagged (\x -> (mempty :: a) .* x == x)


polyprop_act_mult :: (LeftAction g x, Monoid g, Eq x) => g -> g -> x -> Bool
polyprop_act_mult g2 g1 x = (g2 .*. g1) .* x == g2 .* g1 .* x

-- | Matrix multiplication
instance Monoid V.Mat3 where 
    mappend = (V..*.)
    mempty = V.idmtx

-- | Matrix multiplication
instance Monoid V.Mat4 where 
    mappend = (V..*.)
    mempty = V.idmtx

-- | Matrix multiplication
instance Monoid V.Proj4 where 
    mappend = (V..*.)
    mempty = V.idmtx



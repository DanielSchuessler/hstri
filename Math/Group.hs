{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module Math.Group where

import Data.Monoid
import Data.Proxy
import QuickCheckUtil
import Test.QuickCheck
import Data.Semigroup as Semi

(.*.) ::  Semigroup a => a -> a -> a
(.*.) = (Semi.<>)
infixr 7 .*.

polyprop_idl :: (Eq a, Show a, Monoid a, Semigroup a) => a -> Property
polyprop_idl g = 
    printTestCase "Checking monoid left identity law" $
    mempty .*. g .=. g

polyprop_idr :: (Eq a, Show a, Monoid a, Semigroup a) => a -> Property
polyprop_idr g = 
    printTestCase "Checking monoid right identity law" $
    g .*. mempty .=. g

polyprop_assoc :: (Eq a, Show a, Semigroup a) => a -> a -> a -> Property
polyprop_assoc g3 g2 g1 = 
    printTestCase "Checking semigroup associativity law" $
    (g3 .*. g2) .*. g1 .=. g3 .*. (g2 .*. g1)

polyprop_monoid :: (Eq a, Show a, Monoid a, Arbitrary a, Semigroup a) =>Proxy a -> Property
polyprop_monoid p =
    (\x -> polyprop_idl (x `asProxyTypeOf` p)) .&&. 
    (\x -> polyprop_idr (x `asProxyTypeOf` p)) .&&. 
    (\x -> polyprop_assoc (x `asProxyTypeOf` p))

class (Semigroup g, Monoid g) => Group g where
    inv :: g -> g


polyprop_invl ::  (Eq a, Show a, Group a) => a -> Property
polyprop_invl g = 
    printTestCase "Checking group left inverse law" $
    inv g .*. g .=. mempty

polyprop_invr ::  (Eq a, Show a, Group a) => a -> Property
polyprop_invr g = 
    printTestCase "Checking group right inverse law" $
    g .*. inv g .=. mempty

polyprop_group
  :: (Eq a, Show a, Arbitrary a, Group a) => Proxy a -> Property
polyprop_group p =
    polyprop_monoid p .&&.
    (\x -> polyprop_invl (x `asProxyTypeOf` p)) .&&. 
    (\x -> polyprop_invr (x `asProxyTypeOf` p))

-- | Laws:
--
-- * If @g@ is an instance of 'Semigroup', @(g1 <> g2) .* x = g1 .* (g2 .* x)@ 
--
-- * If @g@ is an instance of 'Monoid', @mempty .* x = x@
class LeftAction g x where
    (.*) ::  g -> x -> x

infixr 7 .*

class RightAction g x where
    (*.) ::   x -> g -> x

infixl 7 *.

-- We need a type-level tag here because the monoid isn't inferrable from the acted-upon type
polyprop_lact_id
  :: (Eq a, Show a, Monoid g, LeftAction g a) =>
     Proxy g -> a -> Property
polyprop_lact_id p x = (mempty `asProxyTypeOf` p) .* x .=. x


polyprop_lact_mult :: (Eq a, Show a, Semigroup g, LeftAction g a) =>g -> g -> a -> Property
polyprop_lact_mult g2 g1 x = (g2 .*. g1) .* x .=. g2 .* g1 .* x

polyprop_lact
  :: (Eq a,
      Show g,
      Show a,
      Monoid g,
      Semigroup g,
      Arbitrary g,
      Arbitrary a,
      LeftAction g a) =>
     Proxy (g, a) -> Property
polyprop_lact (p :: Proxy (g,a)) = 
    (\(a::a) -> polyprop_lact_id (fmap fst p) a) .&.
    (\(g2::g) g1 (a::a) -> polyprop_lact_mult g2 g1 a)

-- We need a type-level tag here because the monoid isn't inferrable from the acted-upon type
polyprop_ract_id
  :: (Eq a, Show a, Semigroup g, Monoid g, RightAction g a) =>
     Proxy g -> a -> Property
polyprop_ract_id p x = x *. (mempty `asProxyTypeOf` p) .=. x


polyprop_ract_mult :: (Eq a, Show a, Semigroup g, RightAction g a) =>g -> g -> a -> Property
polyprop_ract_mult g1 g2 x = x *. (g1 .*. g2) .=. x *. g1 *. g2


polyprop_ract
  :: (Eq a,
      Show g,
      Show a,
      Monoid g,
      Semigroup g,
      Arbitrary g,
      Arbitrary a,
      RightAction g a) =>
     Proxy (a, g) -> Property
polyprop_ract (p :: Proxy (a,g)) = 
    (\(a::a) -> polyprop_ract_id (fmap snd p) a) .&.
    (\(g2::g) g1 (a::a) -> polyprop_ract_mult g2 g1 a)


isMonoidHomo :: (Eq a1,Show a,Show a1,Monoid a,Monoid a1,Arbitrary a,Semigroup a,Semigroup a1) =>(a -> a1) -> Property
isMonoidHomo f = 
    (printTestCase "isMonoidHomo: Checking preservation of the identity" $
        f mempty .=. mempty)
    .&&. 
    (printTestCase "isMonoidHomo: Checking preservation of multiplication" $
        property (\x y -> 
            f (x .*. y) .=. f x .*. f y))

-- | Note: Being a monoid homomorphism between groups actually implies being a group homomorphism, but maybe testing the preservation of 'inv' directly catches some bugs that are unlikely to be found by sampling preservation of '.*.'
isGroupHomo
  :: (Eq a1, Show a, Show a1, Arbitrary a, Group a, Group a1) =>
     (a -> a1) -> Property
isGroupHomo f = isMonoidHomo f .&&. 
    (printTestCase "isGroupHomo: Checking preservation of inversion" $
        property (\x -> 
            let
                ix = inv x
                fx = f x
            in
                printTestCase ("inv x = "++show ix) $
                printTestCase ("f x = "++show fx) $
                f ix .=. inv fx))


-- | Shrink to identity 
monoidDefaultShrink :: (Eq a, Monoid a) => a -> [a]
monoidDefaultShrink 
    g | g == mempty = []
      | otherwise = [mempty]


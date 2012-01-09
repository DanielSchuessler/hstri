{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, TypeOperators #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
module GenericsUtil where

import GHC.Generics
import Element
import Control.Monad
import Data.Monoid

class FoldExtract a as where
    foldExtract :: Monoid b => (a -> b) -> as -> b

class GFoldMap a f where
    gFoldExtract :: Monoid b => (a -> b) -> f p -> b

instance GFoldMap a U1 where
    gFoldExtract _ _ = mempty

instance (GFoldMap a f, GFoldMap a g) => GFoldMap a (f :*: g) where
    gFoldExtract inj (f :*: g) = gFoldExtract inj f `mappend` gFoldExtract inj g 

instance (GFoldMap a f, GFoldMap a g) => GFoldMap a (f :+: g) where
    gFoldExtract inj (L1 f) = gFoldExtract inj f
    gFoldExtract inj (R1 g) = gFoldExtract inj g

instance (GFoldMap a f) => GFoldMap a (Rec1 f) where
    gFoldExtract inj (Rec1 f) = gFoldExtract inj f

instance (GFoldMap a f) => GFoldMap a (M1 i c f) where
    gFoldExtract inj (M1 f) = gFoldExtract inj f

instance GFoldMapTypeEq a a' => GFoldMap a (K1 i a') where
    gFoldExtract inj (K1 a) = injIfTypeEq inj a 

class GFoldMapTypeEq a a' where
    injIfTypeEq :: Monoid b => (a -> b) -> a' -> b

instance GFoldMapTypeEq a a where
    injIfTypeEq inj a = inj a

instance FoldExtract a as => GFoldMapTypeEq a as where
    injIfTypeEq = foldExtract

deriveFoldMap
  :: (Generic as, Monoid r, GFoldMap a (Rep as)) =>
     (a -> r) -> as -> r
deriveFoldMap inj = gFoldExtract inj . from

deriveAsList :: (Generic as, GFoldMap a (Rep as)) => as -> [a]
deriveAsList = deriveFoldMap (:[])

instance FoldExtract a [a] where
    foldExtract = deriveFoldMap 

-- | 'const' 'mempty'
instance FoldExtract nota [a] where
    foldExtract = const mempty 

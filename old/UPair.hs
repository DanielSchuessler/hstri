{-# LANGUAGE TypeFamilies, ViewPatterns, DeriveFunctor #-}
module UPair where
import Collections
import Normalizable
import Data.Function
import Element
import Data.Hashable

-- | Unordered pair
data UPair a = UPair a a
    deriving(Functor,Show)

type instance Element (UPair a) = a
instance Ord a => AsList (UPair a) where asList (normalize -> UPair x1 x2) = [x1,x2] 

uPair ::  a -> a -> UPair a
uPair = UPair

uPairFromPair ::  (b, b) -> UPair b
uPairFromPair = uncurry uPair

uPairToPair ::  UPair t -> (t, t)
uPairToPair (UPair x y) = (x,y)

prop_UPair_Eq ::  Ord a => a -> a -> a -> a -> Bool
prop_UPair_Eq x y x' y' = 
    (uPair x y == uPair x' y') 
    == 
    (x==x' && y==y' || x==y' && y==x') 

instance Ord a => Normalizable (UPair a) where
    normalize x@(UPair x1 x2) = if x1 <= x2
                                    then x
                                    else UPair x2 x1

instance Ord a => Eq (UPair a) where (==) = (==) `on` (uPairToPair . normalize)
instance Ord a => Ord (UPair a) where compare = compare `on` (uPairToPair . normalize)


instance (Hashable a, Ord a) => Hashable (UPair a) where hash (normalize -> UPair x1 x2) = hash (x1,x2)

-- instance (Ord a, Repr a) => Repr (UPair a) where 
--     type Rep (UPair a) = (Rep a, Rep a)
--     type RepList (UPair a) = Vector (Rep a,Rep a)
-- 
--     toRep (normalize -> UPair x1 x2) = (toRep x1,toRep x2)
--     toRepList = dToRepList
-- 

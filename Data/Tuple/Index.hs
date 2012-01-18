{-# LANGUAGE FlexibleInstances #-}
module Data.Tuple.Index where
import Test.QuickCheck

data Index3 = I3_0 | I3_1 | I3_2 deriving(Eq,Show)

instance Arbitrary Index3 where arbitrary = elements [I3_0,I3_1,I3_2]
instance CoArbitrary Index3 where coarbitrary = coarbitraryShow
instance Show (Index3 -> Index3) where showsPrec prec f = showParen (prec > 10) (showString "tupleToFun " . shows (tupleFromFun f))                                

tupleToFun :: (t, t, t) -> Index3 -> t
tupleToFun (a,_,_) I3_0 = a
tupleToFun (_,b,_) I3_1 = b
tupleToFun (_,_,c) I3_2 = c

tupleFromFun :: (Index3 -> t) -> (t, t, t)
tupleFromFun f = (f I3_0, f I3_1, f I3_2)



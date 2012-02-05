{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module Data.Tuple.Index where
import Test.QuickCheck
import Element
import Util
import HomogenousTuples

data Index2 = I2_0 | I2_1 deriving(Eq,Show,Enum,Bounded)
data Index3 = I3_0 | I3_1 | I3_2 deriving(Eq,Show,Enum,Bounded)
data Index4 = I4_0 | I4_1 | I4_2 | I4_3 deriving(Eq,Show,Enum,Bounded)

allIndex2 :: [Index2]
allIndex2 = asList allIndex2'

allIndex2' :: (Pair Index2)
allIndex2' = (I2_0,I2_1)

allIndex3' :: Triple Index3
allIndex3' = (I3_0,I3_1,I3_2)

allIndex3 :: [Index3]
allIndex3 = asList allIndex3'

allIndex4' :: (Index4, Index4, Index4, Index4)
allIndex4' = (I4_0,I4_1,I4_2,I4_3)

allIndex4 :: [Index4]
allIndex4 = asList allIndex4'

instance Arbitrary Index2 where arbitrary = elements [minBound..maxBound]
instance CoArbitrary Index2 where coarbitrary = variant . fromEnum
instance Finite Index2

instance Arbitrary Index3 where arbitrary = elements [minBound..maxBound]
instance CoArbitrary Index3 where coarbitrary = variant . fromEnum
instance Finite Index3

instance Arbitrary Index4 where arbitrary = elements [minBound..maxBound]
instance CoArbitrary Index4 where coarbitrary = variant . fromEnum
instance Finite Index4

tupleToFun2 :: (t, t) -> Index2 -> t
tupleToFun2 (a,_) I2_0 = a
tupleToFun2 (_,b) I2_1 = b

tupleToFun3 :: (t, t, t) -> Index3 -> t
tupleToFun3 (a,_,_) I3_0 = a
tupleToFun3 (_,b,_) I3_1 = b
tupleToFun3 (_,_,c) I3_2 = c

tupleToFun4 :: (t, t, t, t) -> Index4 -> t
tupleToFun4 (a,_,_,_) I4_0 = a
tupleToFun4 (_,b,_,_) I4_1 = b
tupleToFun4 (_,_,c,_) I4_2 = c
tupleToFun4 (_,_,_,d) I4_3 = d

tupleFromFun3 :: (Index3 -> t) -> (t, t, t)
tupleFromFun3 f = (f I3_0, f I3_1, f I3_2)

tupleFromFun4 :: (Index4 -> t) -> (t, t, t, t)
tupleFromFun4 f = (f I4_0, f I4_1, f I4_2, f I4_3)

indexOf2' :: Eq a => a -> a -> a -> Maybe Index2
indexOf2' a b x
    | a == x = Just I2_0
    | b == x = Just I2_1
    | otherwise = Nothing

indexOf3' :: Eq a => a -> a -> a -> a -> Maybe Index3
indexOf3' a b c x
    | a == x = Just I3_0
    | b == x = Just I3_1
    | c == x = Just I3_2
    | otherwise = Nothing

indexOf4' :: Eq a => a -> a -> a -> a -> a -> Maybe Index4
indexOf4' a b c d x
    | a == x = Just I4_0
    | b == x = Just I4_1
    | c == x = Just I4_2
    | d == x = Just I4_3
    | otherwise = Nothing

indexOf2 :: Eq a => (a, a) -> a -> Maybe Index2
indexOf2 (a,b) = indexOf2' a b

indexOf3 :: Eq a => (a, a, a) -> a -> Maybe Index3
indexOf3 (a,b,c) = indexOf3' a b c

indexOf4 :: Eq a => (a, a, a, a) -> a -> Maybe Index4
indexOf4 (a,b,c,d) = indexOf4' a b c d


instance Show (Index3 -> Index3) where 
    showsPrec prec f = showParen (prec > 10) 
        (showString "tupleToFun3 " . shows (tupleFromFun3 f))                                

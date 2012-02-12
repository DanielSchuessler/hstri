{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies,  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
module Data.BitVector.Adaptive where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed(Vector,Unbox)
import Data.Bits
import Data.Proxy
import Data.Word
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.Mutable as VUM
import Debug.Trace
import Data.Function
import VectorUtil
import FileLocation
import Data.Typeable
import Control.Arrow

#define BITSIZE(X) (bitSize (undefined :: X))
#define FBVSIZE(X) (fbvSize (undefined :: Proxy (X)))

type BitVectorPosition = Int

class BitVector w where
    bvUnsafeIndex :: w -> BitVectorPosition -> Bool
    bvIntersection :: w -> w -> w
    bvUnion :: w -> w -> w
    bvComplement :: w -> w
    bvSetBit :: w -> BitVectorPosition -> w
    bvClearBit :: w -> BitVectorPosition -> w

    -- | Vector of the given length, with all but the given bit set to 1
    bvAllBut :: Int -> BitVectorPosition -> w 

    bvSubset :: w -> w -> Bool

    bvToVector :: w -> Vector Bool

    -- | Initialize a constant-0 bitvector of size /at least/ the given size (no check)
    bvUnsafeEmpty :: Int -> w

    -- | Initialize a constant-1 bitvector of size /at least/ the given size (no check)
    bvUnsafeFull :: Int -> w

    -- | Max size supported by the type. Must be @Just 'fbvSize'@ if @w@ is an instance of 'FixedBitVector'.
    -- @Nothing@ indicates that the type supports arbitrary width.
    bvMaxBitSize :: Proxy w -> Maybe Int

    -- | Note: This may be larger than the size passed to a constructor function (fixed-size bitvectors don't remember their \"actual\" size)
    bvBitSize :: w -> Int


class BitVector w => FixedBitVector w where
    fbvSize :: Proxy w -> Int
    fbvEmpty :: w
    fbvFull :: w
    fbvAllBut :: BitVectorPosition -> w

bvIsLeqMaxSize :: BitVector w => Int -> Proxy w -> Bool
bvIsLeqMaxSize n = maybe True (>=n) . bvMaxBitSize

-- | Initialize a constant-0 bitvector of size /at least/ the given size
bvEmpty :: BitVector w => Int -> w
bvEmpty n = go undefined
    where
        go :: BitVector w => Proxy w -> w
        go p = if bvIsLeqMaxSize n p
               then bvUnsafeEmpty n
               else $err' ("bvEmpty" ++ show n) 

-- | Initialize a constant-1 bitvector of size /at least/ the given size
bvFull :: BitVector w => Int -> w
bvFull n = go undefined
    where
        go :: BitVector w => Proxy w -> w
        go p = if bvIsLeqMaxSize n p
               then bvUnsafeFull n
               else $err' ("bvFull" ++ show n) 

instance (Unbox w, FixedBitVector w) => BitVector (Vector w) where
    bvUnsafeIndex v (flip divMod FBVSIZE(w) -> (i,j)) 

        = bvUnsafeIndex (VU.unsafeIndex v i) j

    bvIntersection = VU.zipWith bvIntersection
    bvUnion = VU.zipWith bvUnion
    bvComplement = VU.map bvComplement


    bvAllBut n (flip divMod FBVSIZE(w) -> (i,j)) 

        = VU.generate n (\i' -> if i'==i then fbvAllBut j else fbvFull) 
                        

    bvSubset x y = VU.and $ VU.zipWith bvSubset x y

    bvToVector = VU.concatMap bvToVector

    bvUnsafeEmpty n = VU.replicate n fbvEmpty 

    bvUnsafeFull n = VU.replicate n fbvFull 

    bvMaxBitSize = const Nothing

    bvSetBit v (flip divMod FBVSIZE(w) -> (i,j))   = vectorAdjustAt v (flip bvSetBit j) i
    bvClearBit v (flip divMod FBVSIZE(w) -> (i,j)) = vectorAdjustAt v (flip bvClearBit j) i 
    bvBitSize = VU.length


deriving instance VG.Vector Vector w => VG.Vector Vector (BitVectorSingle w)
deriving instance VGM.MVector VUM.MVector w => VGM.MVector VUM.MVector (BitVectorSingle w)
deriving instance Unbox w => Unbox (BitVectorSingle w)

newtype BitVectorSingle w = BitVectorSingle w 
    deriving(Eq,Ord,Num,Bits,Typeable)


bvShow :: BitVector w => w -> String
bvShow w = map ((\b -> if b then '1' else '0') . bvUnsafeIndex w) [bvBitSize w-1,bvBitSize w-2 .. 0]


instance Bits w => Show (BitVectorSingle w) where
    show = bvShow

instance Bits w => BitVector (BitVectorSingle w) where
    bvUnsafeIndex = testBit
    bvIntersection = (.&.)
    bvUnion = (.|.)
    bvComplement = complement
    bvSetBit = setBit
    bvClearBit = clearBit

    bvSubset x y = (x .&. y) == x

    bvToVector x = VU.generate BITSIZE(w) (testBit x) 

    bvAllBut = const fbvAllBut
    bvUnsafeEmpty = const fbvEmpty
    bvUnsafeFull = const fbvFull
    bvMaxBitSize = Just . fbvSize
    bvBitSize = const BITSIZE(w)
    
instance Bits w => FixedBitVector (BitVectorSingle w) where
    fbvSize = const BITSIZE(w)
    fbvEmpty = 0
    fbvFull = complement 0
    fbvAllBut k = clearBit fbvFull k


type Hi w = w
type Lo w = w

class Doubleable w where
    data Doubled w

    dbl_hi :: Doubled w -> Hi w 
    dbl_lo :: Doubled w -> Lo w
    dbl_new :: Hi w -> Lo w -> Doubled w


dbl_zipWith
  :: (Doubleable c, Doubleable b) =>
     (b -> b -> c) -> Doubled b -> Doubled b -> Doubled c
dbl_zipWith f x y = dbl_new ((f `on` dbl_hi) x y) ((f `on` dbl_lo) x y)

dbl_map
  :: (Doubleable w, Doubleable w1) =>
     (w1 -> w) -> Doubled w1 -> Doubled w
dbl_map f x = dbl_new (f (dbl_hi x)) (f (dbl_lo x))

-- | Returns the most significant, then the least significant part
dbl_toTuple :: Doubleable c' => Doubled c' -> (Hi c', Lo c')
dbl_toTuple = dbl_hi &&& dbl_lo


dbl_setOrClearBit :: forall w. (Doubleable w, FixedBitVector w) => 
    (w -> Int -> w) -> Doubled w -> Int -> Doubled w
dbl_setOrClearBit what x n =
        case n - FBVSIZE(w) of
             n' | n' < 0 ->    dbl_new  (dbl_hi x) (what (dbl_lo x) n)
                | otherwise -> dbl_new  (what (dbl_hi x) n') (dbl_lo x)

instance (Doubleable w, FixedBitVector w) => Show (Doubled w) where
    show = bvShow

instance (Doubleable w, FixedBitVector w) => BitVector (Doubled w) where
    bvUnsafeIndex x n = 
        case n - FBVSIZE(w) of
             n' | n' < 0 ->    bvUnsafeIndex (dbl_lo x) n 
                | otherwise -> bvUnsafeIndex (dbl_hi x) n' 

    bvSetBit = dbl_setOrClearBit bvSetBit
    bvClearBit = dbl_setOrClearBit bvClearBit


    bvIntersection = dbl_zipWith bvIntersection
    bvUnion = dbl_zipWith bvUnion
    bvComplement = dbl_map bvComplement

    bvSubset x y = (bvSubset `on` dbl_hi) x y &&
                   (bvSubset `on` dbl_lo) x y

    bvToVector x = bvToVector (dbl_hi x) VG.++ bvToVector (dbl_lo x)

    bvAllBut = const fbvAllBut
    bvUnsafeEmpty = const fbvEmpty
    bvUnsafeFull = const fbvFull
    bvMaxBitSize = Just . fbvSize
    bvBitSize = const (2*FBVSIZE(w))
    
instance (Doubleable w, FixedBitVector w) => FixedBitVector (Doubled w) where
    fbvSize = const (2*FBVSIZE(w))
    fbvEmpty = dbl_new fbvEmpty fbvEmpty
    fbvFull = dbl_new fbvFull fbvFull
    fbvAllBut n = 
    
        case n - FBVSIZE(w) of
             n' | n' < 0 ->    dbl_new fbvFull (fbvAllBut n)
                | otherwise -> dbl_new (fbvAllBut n') fbvFull


instance (Doubleable w, Eq w) => Eq (Doubled w) where
    (==) = (==) `on` dbl_toTuple

instance (Doubleable w, Ord w) => Ord (Doubled w) where
    compare = compare `on` dbl_toTuple
    
instance Doubleable (BitVectorSingle Word64) where
    data Doubled (BitVectorSingle Word64) = BitVector128 {
        bv128_hi :: {-# UNPACK #-} !(BitVectorSingle Word64),
        bv128_lo :: {-# UNPACK #-} !(BitVectorSingle Word64) 
    }
        deriving Typeable

    dbl_hi = bv128_hi
    dbl_lo = bv128_lo
    dbl_new = BitVector128

type BitVector64 = BitVectorSingle Word64
type BitVector128 = Doubled (BitVectorSingle Word64)
    

{-# INLINE withBitVectorType #-}
withBitVectorType :: Int -> (forall w. (BitVector w, Show w, Typeable w, Eq w, Ord w) => Proxy w -> r) -> r
withBitVectorType size_ k
    | size_ <= FBVSIZE(BitVectorSingle Word) = k (undefined :: Proxy (BitVectorSingle Word))
    | size_ <= FBVSIZE(BitVector64)          = k (undefined :: Proxy (BitVector64))
    | size_ <= FBVSIZE(BitVector128)         = k (undefined :: Proxy (BitVector128))
    | otherwise                              = 
        
        trace ("withBitVectorType: Notice: Using vector type (requested size = "++show size_++")") $ 
    
        k (undefined :: Proxy (Vector (BitVectorSingle Word)))



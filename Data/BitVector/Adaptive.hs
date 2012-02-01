{-# LANGUAGE TypeFamilies,  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
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
import Control.Exception
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed.Mutable as VUM
import Debug.Trace
import Data.Function

#define BITSIZE(X) (bitSize (undefined :: X))
#define FBVSIZE(X) (fbvSize (undefined :: Proxy (X)))

type BitVectorPosition = Int

class BitVector w where
    bvUnsafeIndex :: w -> BitVectorPosition -> Bool
    bvIntersection :: w -> w -> w
    bvUnion :: w -> w -> w
    bvComplement :: w -> w

    -- | Vector of the given length, with all but the given bit set to 1
    bvAllBut :: Int -> BitVectorPosition -> w 

    bvSubset :: w -> w -> Bool

    bvToVector :: w -> Vector Bool

    -- | Initialize a constant-0 bitvector of size /at least/ the given size (no check)
    bvUnsafeEmpty :: Int -> w

    -- | Initialize a constant-1 bitvector of size /at least/ the given size (no check)
    bvUnsafeFull :: Int -> w

    -- | Must be @Just 'fbvSize'@ if @w@ is an instance of 'FixedBitVector'.
    -- @Nothing@ indicates that @w@ is arbitrary-width.
    bvMaxSize :: Proxy w -> Maybe Int

class BitVector w => FixedBitVector w where
    fbvSize :: Proxy w -> Int
    fbvEmpty :: w
    fbvFull :: w
    fbvAllBut :: BitVectorPosition -> w

bvIsLeqMaxSize :: BitVector w => Int -> Proxy w -> Bool
bvIsLeqMaxSize n = maybe True (>=n) . bvMaxSize

-- | Initialize a constant-0 bitvector of size /at least/ the given size
bvEmpty :: BitVector w => Int -> w
bvEmpty n = go undefined
    where
        go :: BitVector w => Proxy w -> w
        go p = assert (bvIsLeqMaxSize n p) (bvUnsafeEmpty n)

-- | Initialize a constant-1 bitvector of size /at least/ the given size
bvFull :: BitVector w => Int -> w
bvFull n = go undefined
    where
        go :: BitVector w => Proxy w -> w
        go p = assert (bvIsLeqMaxSize n p) (bvUnsafeFull n)

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

    bvMaxSize = const Nothing


deriving instance VG.Vector Vector w => VG.Vector Vector (BitVectorSingle w)
deriving instance VGM.MVector VUM.MVector w => VGM.MVector VUM.MVector (BitVectorSingle w)
deriving instance Unbox w => Unbox (BitVectorSingle w)

newtype BitVectorSingle w = BitVectorSingle w 
    deriving(Eq,Ord,Show,Num,Bits)



instance Bits w => BitVector (BitVectorSingle w) where
    bvUnsafeIndex = testBit
    bvIntersection = (.&.)
    bvUnion = (.|.)
    bvComplement = complement

    bvSubset x y = (x .&. y) == x

    bvToVector x = VU.generate BITSIZE(w) (testBit x) 

    bvAllBut = const fbvAllBut
    bvUnsafeEmpty = const fbvEmpty
    bvUnsafeFull = const fbvFull
    bvMaxSize = Just . fbvSize
    
instance Bits w => FixedBitVector (BitVectorSingle w) where
    fbvSize = const BITSIZE(w)
    fbvEmpty = 0
    fbvFull = complement 0
    fbvAllBut k = clearBit fbvFull k



class Doubleable w where
    data Doubled w

    dbl_hi, dbl_lo :: Doubled w -> w
    dbl_new :: w -> w -> Doubled w


dbl_zipWith
  :: (Doubleable c, Doubleable b) =>
     (b -> b -> c) -> Doubled b -> Doubled b -> Doubled c
dbl_zipWith f x y = dbl_new ((f `on` dbl_hi) x y) ((f `on` dbl_lo) x y)

dbl_map
  :: (Doubleable w, Doubleable w1) =>
     (w1 -> w) -> Doubled w1 -> Doubled w
dbl_map f x = dbl_new (f (dbl_hi x)) (f (dbl_lo x))

instance Doubleable (BitVectorSingle Word64) where
    data Doubled (BitVectorSingle Word64) = BitVector128 {
        bv128_hi :: {-# UNPACK #-} !(BitVectorSingle Word64),
        bv128_lo :: {-# UNPACK #-} !(BitVectorSingle Word64) 
    }

    dbl_hi = bv128_hi
    dbl_lo = bv128_lo
    dbl_new = BitVector128

instance (Doubleable w, FixedBitVector w) => BitVector (Doubled w) where
    bvUnsafeIndex x n = 
        case n - FBVSIZE(w) of
             n' | n' < 0 ->    bvUnsafeIndex (dbl_lo x) n 
                | otherwise -> bvUnsafeIndex (dbl_hi x) n' 

    bvIntersection = dbl_zipWith bvIntersection
    bvUnion = dbl_zipWith bvUnion
    bvComplement = dbl_map bvComplement

    bvSubset x y = (bvSubset `on` dbl_hi) x y &&
                   (bvSubset `on` dbl_lo) x y

    bvToVector x = bvToVector (dbl_hi x) VG.++ bvToVector (dbl_lo x)

    bvAllBut = const fbvAllBut
    bvUnsafeEmpty = const fbvEmpty
    bvUnsafeFull = const fbvFull
    bvMaxSize = Just . fbvSize
    
instance (Doubleable w, FixedBitVector w) => FixedBitVector (Doubled w) where
    fbvSize = const (2*FBVSIZE(w))
    fbvEmpty = dbl_new fbvEmpty fbvEmpty
    fbvFull = dbl_new fbvFull fbvFull
    fbvAllBut n = 
    
        case n - FBVSIZE(w) of
             n' | n' < 0 ->    dbl_new fbvFull (fbvAllBut n)
                | otherwise -> dbl_new (fbvAllBut n') fbvFull
    
    
type BitVector64 = BitVectorSingle Word64
type BitVector128 = Doubled (BitVectorSingle Word64)
    

{-# INLINABLE withBitVectorType #-}
withBitVectorType :: Int -> (forall w. BitVector w => Proxy w -> r) -> r
withBitVectorType size_ k
    | size_ <= FBVSIZE(BitVectorSingle Word) = k (undefined :: Proxy (BitVectorSingle Word))
    | size_ <= FBVSIZE(BitVector64)          = k (undefined :: Proxy (BitVector64))
    | size_ <= FBVSIZE(BitVector128)         = k (undefined :: Proxy (BitVector128))
    | otherwise                              = 
        
        trace ("withBitVectorType: Notice: Using vector type (requested size = "++show size_++")") $ 
    
        k (undefined :: Proxy (Vector (BitVectorSingle Word)))



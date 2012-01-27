{-# LANGUAGE  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
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

#define BITSIZE(X) (bitSize (undefined :: X))
#define FBVSIZE(X) (fbvSize (undefined :: Proxy (X)))

type BitVectorPosition = Int

class BitVector w where
    bvUnsafeIndex :: w -> BitVectorPosition -> Bool
    bvIntersect :: w -> w -> w

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
    bvUnsafeIndex v (divMod FBVSIZE(w) -> (i,j)) 

        = bvUnsafeIndex (VU.unsafeIndex v i) j

    bvIntersect = VU.zipWith bvIntersect

    bvAllBut n (divMod FBVSIZE(w) -> (i,j)) 

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
    bvIntersect = (.&.)

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


{-# INLINABLE withBitVectorType #-}
withBitVectorType :: Int -> (forall w. BitVector w => Proxy w -> r) -> r
withBitVectorType size_ k
    | size_ <= bitSize BITSIZE(Word)   = k (undefined :: Proxy (BitVectorSingle Word))
    | size_ <= bitSize BITSIZE(Word64) = k (undefined :: Proxy (BitVectorSingle Word64))
    | otherwise                        = k (undefined :: Proxy (Vector (BitVectorSingle Word)))



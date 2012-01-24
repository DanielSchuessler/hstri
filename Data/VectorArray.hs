{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor #-}
module Data.VectorArray where

import Data.Vector.Generic(Vector)
import qualified Data.Vector.Generic as VG
import Data.Ix
import Control.Arrow

data VArray ix v a = 
    VArray { 
        va_lower, va_upper :: ix,
        va_vec :: v a
    }
    deriving (Functor,Show,Eq,Ord)

va_bounds :: VArray ix v a -> (ix, ix)
va_bounds = va_lower &&& va_upper

va_indices :: Ix ix => VArray ix v a -> [ix]
va_indices v = range (va_lower v, va_upper v) 

va_map :: (Vector v a, Vector v b) => (a -> b) -> VArray ix v a -> VArray ix v b
va_map f (VArray l u v) = VArray l u (VG.map f v) 

va_convert :: (Vector u a, Vector v a) => VArray ix u a -> VArray ix v a
va_convert (VArray l u v) = VArray l u (VG.convert v) 

va_index :: (Ix ix, Vector v a) => VArray ix v a -> ix -> a
va_index v i = va_vec v VG.! index (va_bounds v) i 

va_unsafeIndex :: (Ix ix, Vector v a) => VArray ix v a -> ix -> a
va_unsafeIndex v i = va_vec v `VG.unsafeIndex` index (va_bounds v) i 

va_identity :: (Ix ix, Vector v ix) => (ix, ix) -> VArray ix v ix
va_identity r = va_fromList r (range r)

va_fromList :: (Ix ix, Vector v a) => (ix, ix) -> [a] -> VArray ix v a
va_fromList (l,u) = VArray l u . VG.fromListN (rangeSize (l,u)) 

va_elems :: Vector v a => VArray ix v a -> [a]
va_elems = VG.toList . va_vec 

-- | = 'va_elems'
va_toList = va_elems

va_assocs :: (Ix ix, Vector v a) => VArray ix v a -> [(ix, a)]
va_assocs v = zip (range (va_bounds v)) (va_elems v) 


--va_upperBound =  VG.maxIndex 

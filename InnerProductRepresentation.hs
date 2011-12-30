{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module InnerProductRepresentation where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import PrettyUtil
import Control.Exception

newtype VectorIndex = VectorIndex Int
    deriving(Eq,Enum,Ord,Real,Num,Integral)

instance Show VectorIndex where
    show (VectorIndex i) = pad 3 . show $ i

instance Pretty VectorIndex where
    pretty = text . show

data IPR = IPR {
    ipr_index :: VectorIndex,
    zeroSet :: VU.Vector Bool,
    innerProducts :: V.Vector Rational
}

ipr_head :: IPR -> Rational
ipr_head = V.head . innerProducts

ipr_tail :: IPR -> IPR
ipr_tail x = x { innerProducts = V.tail (innerProducts x) }

ipr_combine :: IPR -> IPR -> VectorIndex -> IPR
ipr_combine x y index = 
    let
        ipsx = innerProducts x
        ipsy = innerProducts y

        hx = V.head ipsx
        hy = V.head ipsy
    in
        assert (hx > 0 && hy < 0) $
        IPR 
            index
            (VU.zipWith (&&) (zeroSet x) (zeroSet y)) 
            (V.zipWith 
                (\xi yi -> (hy*xi-hx*yi)    
                             /(hx-hy))
                (V.tail ipsx)
                (V.tail ipsy))

instance Pretty IPR where
    pretty (IPR i z ips) = 
            hencloseSep lparen rparen (text " , ")
                [   pretty i
                ,   VU.ifoldr f empty z
                ,   V.foldr g empty ips
                ]
        where
            f j b r = 
                            (if j /= 0 && mod j 3 == 0 then space else empty)
                        <>  char (if b then '0' else '.') 
                        <>  r

            g (showRational -> x) r =
                text (pad 6 x ++ "  ") <> r


instance Show IPR where
    showsPrec = prettyShowsPrec
--     showsPrec _ (IPR i z ips) = showChar '(' . shows i . showString " , " 
--                                     . VU.foldr f id z . showString " , " 
--                                     . shows ips . showChar ')'
--         where
--             f b r = showChar (if b then '0' else '.') . r
-- 

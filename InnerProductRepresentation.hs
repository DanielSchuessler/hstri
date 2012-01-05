{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, ViewPatterns, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module InnerProductRepresentation where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import PrettyUtil
import Control.Exception
import Data.Function

#define IPR_WITHVALUES

newtype VectorIndex = VectorIndex Int
    deriving(Eq,Enum,Ord,Real,Num,Integral)

instance Show VectorIndex where
    show (VectorIndex i) = pad 3 . show $ i

instance Pretty VectorIndex where
    pretty = dullyellow . text . show

data IPR = IPR {
    ipr_index :: VectorIndex,
    zeroSet :: VU.Vector Bool,
    innerProducts :: V.Vector Rational
#ifdef IPR_WITHVALUES
    , ipr_value :: V.Vector Rational
#endif
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

        c = (\xi yi -> (hy*xi-hx*yi)    
                             /(hx-hy))
    in
        assert (hx > 0 && hy < 0) $
        IPR 
            index
            (VU.zipWith (&&) (zeroSet x) (zeroSet y)) 
            (V.zipWith c
                (V.tail ipsx)
                (V.tail ipsy))

#ifdef IPR_WITHVALUES
            ((V.zipWith c `on` ipr_value) x y)
#endif

instance Pretty IPR where
    pretty = ipr_pretty 6 
    
    
    
ipr_pretty :: Int -> IPR -> Doc
ipr_pretty scalarWidth ipr@IPR { ipr_index = i, zeroSet = z, innerProducts = ips } = 
            hencloseSep lparen rparen (text " , ")
                [   pretty i
                ,   VU.ifoldr f empty z
                ,   V.foldr g empty ips
#ifdef IPR_WITHVALUES
                ,   V.foldr g empty (ipr_value ipr)
#endif
                ]
        where
            f j b r = 
                            (if j /= 0 && mod j 3 == 0 then space else empty)
                        <>  char (if b then '0' else '.') 
                        <>  r

            g (showRational -> x) r =
                text (pad scalarWidth x ++ " ") <> r

ipr_maxScalarWidth :: IPR -> Int
ipr_maxScalarWidth ipr = 
        V.maximum (V.snoc (V.map f (innerProducts ipr)) 0)
#ifdef IPR_WITHVALUES
        `max`
        V.maximum (V.map f (ipr_value ipr))
#endif

    where
        f = length . showRational


instance Show IPR where
    showsPrec = prettyShowsPrec


--     showsPrec _ (IPR i z ips) = showChar '(' . shows i . showString " , " 
--                                     . VU.foldr f id z . showString " , " 
--                                     . shows ips . showChar ')'
--         where
--             f b r = showChar (if b then '0' else '.') . r
-- 

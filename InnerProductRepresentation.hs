{-# LANGUAGE MultiParamTypeClasses, CPP, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module InnerProductRepresentation where

import Control.Exception
import Data.Function
import Data.Semigroup
import MathUtil
import OrphanInstances()
import PrettyUtil
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.BitVector.Adaptive
import CoordSys
import VerboseDD.Types

#define IPR_WITHVALUES


data IPR co w = IPR {
    ipr_index :: {-# UNPACK #-} !VectorIndex,
    zeroSet :: !(ZeroSet co w),
    innerProducts :: {-# UNPACK #-} !(V.Vector Rational)
#ifdef IPR_WITHVALUES
    , ipr_value :: (V.Vector Rational)
#endif
}

ipr_head :: IPR co w -> Rational
ipr_head = V.head . innerProducts

ipr_tail :: IPR co w -> IPR co w
ipr_tail x = x { innerProducts = V.tail (innerProducts x) }

ipr_combine :: BitVector w => IPR co w -> IPR co w -> VectorIndex -> IPR co w
ipr_combine x y index = 
    let
        ipsx = innerProducts x
        ipsy = innerProducts y

        hx = V.head ipsx
        hy = V.head ipsy

        c = (\xi yi -> (hy*xi-hx*yi)    
                             /(hy-hx))
    in
        assert (hx > 0 && hy < 0) $
        IPR  
            index
            (bvIntersection (zeroSet x) (zeroSet y)) 
            (V.zipWith c
                (V.tail ipsx)
                (V.tail ipsy))

#ifdef IPR_WITHVALUES
            ((V.zipWith c `on` ipr_value) x y)
#endif

instance (BitVector w) => Pretty (IPR co w) where
    pretty = ipr_pretty ShowZeros 6 
    
    
    
ipr_pretty :: BitVector w => ZeroPrinting -> Int -> IPR co w -> Doc
ipr_pretty zp scalarWidth ipr@IPR { ipr_index = i, zeroSet = z, innerProducts = ips } = 
            hencloseSep lparen rparen (text " , ")
                [   pretty i
                ,   VU.ifoldr f empty (bvToVector z)
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

            g (showRational zp -> x) r =
                text (pad scalarWidth x ++ " ") <> r

ipr_maxScalarWidth :: IPR co w -> Int
ipr_maxScalarWidth ipr = 
        V.maximum (V.snoc (V.map f (innerProducts ipr)) 0)
#ifdef IPR_WITHVALUES
        `max`
        V.maximum (V.map f (ipr_value ipr))
#endif

    where
        f = length . showRational ShowZeros


instance BitVector w => Show (IPR co w) where
    showsPrec = prettyShowsPrec


--     showsPrec _ (IPR co w i z ips) = showChar '(' . shows i . showString " , " 
--                                     . VU.foldr f id z . showString " , " 
--                                     . shows ips . showChar ')'
--         where
--             f b r = showChar (if b then '0' else '.') . r
-- 
--

ipr_makeIntegral :: IPR co w -> IPR co w
ipr_makeIntegral ipr =

    ipr 
#ifdef IPR_WITHVALUES
        { 
            ipr_value = V.map toRational . makeVecIntegral . ipr_value $ ipr
        }

#endif


instance BitVector w => Pretty (PairFate (IPR co w)) where
    pretty (PairFate x y z) = 
        string "P" <> 
            (parens $ hsep (punctuate comma 
                [ pretty (ipr_index x), pretty (ipr_index y), pretty z ]))

instance (BitVector w) => VerboseDDVectorRepresentation (IPR co w) co w where

    ddrep_index = ipr_index
    ddrep_zeroSet = zeroSet



{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module ONormal where

import Collections
import Element
import NormalDisc
import HomogenousTuples
import Text.PrettyPrint.ANSI.Leijen

-- | Ordered 'NormalArc'
newtype ONormalArc = ONormalArc (Pair NormalCorner) 
    deriving(LeftAction S2, MakeNormalArc, Eq, Ord, Pretty, Show)

class MakeONormalArc a where
    oNormalArc :: a -> ONormalArc 

class AsList oNormalArcTuple => HasONormalArcs a oNormalArcTuple | a -> oNormalArcTuple where
    oNormalArcs :: a -> oNormalArcTuple

instance MakeONormalArc (Pair NormalCorner) where
    oNormalArc = ONormalArc

instance MakeONormalArc (Pair Edge) where
    oNormalArc = oNormalArc . map2 normalCorner 

instance HasNormalCorners ONormalArc (Pair NormalCorner) where
    normalCorners (ONormalArc x) = x

instance HasONormalArcs Triangle (Triple NormalArc) where
    oNormalArcs (normalCorners -> (x0,x1,x2)) = (normalArc (x2,x0), normalArc (x0,x1), normalArc (x1,x2))

instance HasONormalArcs OTriangle (Triple NormalArc) where
    oNormalArcs (normalCorners -> (x0,x1,x2)) = (normalArc (x2,x0), normalArc (x0,x1), normalArc (x1,x2))


deriveCollectionKeyClass ''ONormalArc

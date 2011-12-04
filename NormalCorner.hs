{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module NormalCorner(
         -- * Normal corners
        NormalCorner, allNormalCorners, allNormalCorners', normalCornerGetContainingEdge, 
        MakeNormalCorner(..),
        NormalCorners(..),
        normalCornerList,
                   )

                   where

import AbstractTetrahedron
import Element
import PrettyUtil
import Test.QuickCheck
import HomogenousTuples

class AsList normalCornerTuple => NormalCorners a normalCornerTuple | a -> normalCornerTuple where
    normalCorners :: a -> normalCornerTuple


normalCornerList ::  NormalCorners a normalCornerTuple => a -> [Element normalCornerTuple]
normalCornerList = asList . normalCorners

newtype NormalCorner = NormalCorner Edge 
    deriving (Eq,Ord,Enum,Bounded,Arbitrary)

class MakeNormalCorner a where
    normalCorner :: a -> NormalCorner

instance Show NormalCorner where
    showsPrec = prettyShowsPrec

instance Pretty NormalCorner where 
    pretty (NormalCorner e) = green (lbrace <>
        text "Corner on"
        <+> pretty e <> rbrace)

allNormalCorners' :: (Sextuple NormalCorner)
allNormalCorners' = map6 NormalCorner allEdges'

allNormalCorners :: [NormalCorner]
allNormalCorners = asList allNormalCorners'

normalCornerGetContainingEdge ::  NormalCorner -> Edge
normalCornerGetContainingEdge (NormalCorner e) = e

instance MakeEdge NormalCorner where
    edge = normalCornerGetContainingEdge


instance MakeNormalCorner Edge where
    normalCorner = NormalCorner

instance IsSubface NormalCorner Edge where
    isSubface nct e = normalCornerGetContainingEdge nct == e 

instance IsSubface NormalCorner Triangle where
    isSubface nct t = normalCornerGetContainingEdge nct `isSubface` t

instance NormalCorners Triangle (Triple NormalCorner) where
    normalCorners = map3 normalCorner . edges

instance NormalCorners OTriangle (Triple NormalCorner) where
    normalCorners = map3 normalCorner . edges

instance MakeNormalCorner OEdge where
    normalCorner = normalCorner . forgetVertexOrder


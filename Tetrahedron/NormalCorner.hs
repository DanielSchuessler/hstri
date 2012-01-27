{-# LANGUAGE ViewPatterns, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module Tetrahedron.NormalCorner(
         -- * Normal corners
        NormalCorner, allNormalCorners, allNormalCorners', normalCornerGetContainingEdge, 
        MakeNormalCorner(..),
        NormalCorners(..),
        normalCornerList,
                   )

                   where

import Tetrahedron
import Element
import PrettyUtil
import Test.QuickCheck
import HomogenousTuples
import Language.Haskell.TH.Syntax
import Quote
import Control.DeepSeq


class AsList normalCornerTuple => NormalCorners a normalCornerTuple | a -> normalCornerTuple where
    normalCorners :: a -> normalCornerTuple


normalCornerList ::  NormalCorners a normalCornerTuple => a -> [Element normalCornerTuple]
normalCornerList = asList . normalCorners

newtype NormalCorner = NormalCorner Edge 
    deriving (Eq,Ord,Enum,Bounded,Arbitrary,NFData)

class MakeNormalCorner a where
    normalCorner :: a -> NormalCorner

instance Show NormalCorner where
    showsPrec = prettyShowsPrec

instance Pretty NormalCorner where 
    pretty = green . text . quote

allNormalCorners' :: (Sextuple NormalCorner)
allNormalCorners' = map6 NormalCorner allEdges'

allNormalCorners :: [NormalCorner]
allNormalCorners = asList allNormalCorners'

normalCornerGetContainingEdge ::  NormalCorner -> Edge
normalCornerGetContainingEdge (NormalCorner e) = e

instance MakeEdge NormalCorner where
    edge = normalCornerGetContainingEdge


-- | Normal corner on the given edge
instance MakeNormalCorner Edge where
    normalCorner = NormalCorner

-- | Normal corner on the edge joining the two vertices
instance MakeNormalCorner (Vertex,Vertex) where
    normalCorner = normalCorner . edge

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

instance Lift NormalCorner where
    lift (NormalCorner e) = [| NormalCorner e |]

instance Quote NormalCorner where
    quote (NormalCorner (vertices -> (v0,v1))) = "nc"++show v0++show v1


{-# LANGUAGE FlexibleContexts, StandaloneDeriving, NoMonomorphismRestriction, FlexibleInstances, TemplateHaskell, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances, ViewPatterns #-}
{-# OPTIONS -Wall #-}
module Tetrahedron(
    module Math.Groups.S2,
    module Math.Groups.S3,
    module TIndex,
    module Data.Monoid,
    module Tetrahedron.Vertex,
    module Tetrahedron.Edge,
    module Tetrahedron.Triangle,
    module AbsTet,
    module Tetrahedron.Constants,
    module OrderableFace,
    module Data.Word,

    -- * Ordered faces
    IsSubface(..),liftIsSubface,
    Intersection(..),

   )
    
    where

import Tetrahedron.Constants
import AbsTet
import Control.Exception
import Control.Monad.RWS
import HomogenousTuples
import Prelude hiding(catch,lookup)
import Math.Groups.S2
import Math.Groups.S3
import Tetrahedron.Vertex
import Tetrahedron.Edge
import Tetrahedron.Triangle
import TIndex
import OrderableFace
import Data.Monoid(Monoid(..),mconcat)
import Data.Word
import qualified Data.List as L
import Control.Arrow((+++))
import Data.SumType
import Simplicial.DeltaSet1
import Simplicial.DeltaSet2




class Intersection a b c | a b -> c where
    intersect :: a -> b -> c

instance Eq a => Intersection [a] [a] [a] where
    intersect = L.intersect


-- | Equivalent to 'intersectEdges'
instance Intersection Edge Edge (Maybe (Either Edge Vertex)) where
    intersect = intersectEdges 

instance Intersection IEdge IEdge (Maybe (Either IEdge IVertex))  where
    intersect (viewI -> I i x) (viewI -> I i' x') = 
        if (i==i')
           then fmap ((i ./) +++ (i ./)) (intersect x x')
           else Nothing
         

instance Intersection Triangle Triangle (Either Triangle Edge) where
    intersect t1 t2 = case filter3 (\v -> elem3 v (edges t2)) (edges t1) of
                           [e] -> assert (t1/=t2) (Right e)
                           [_,_,_] -> assert (t1==t2) (Left t1)
                           _ -> assert False (error "impossible")













                            









-- instance Enum OEdge where
--     toEnum n = case toEnum n of EnumPair a b -> OEdge a b
--     fromEnum (OEdge a b) = fromEnum (EnumPair a b)


class IsSubface x y where
    isSubface :: x -> y -> Bool

-- | Equivalent to 'isVertexOfEdge'
instance IsSubface Vertex Edge where
    isSubface = isVertexOfEdge

-- | Equivalent to 'isVertexOfTriangle'
instance IsSubface Vertex Triangle where
    isSubface = isVertexOfTriangle

-- | Equivalent to 'isEdgeOfTriangle'
instance IsSubface Edge Triangle where
    isSubface = isEdgeOfTriangle

instance IsSubface OEdge Triangle where
    isSubface = isSubface . forgetVertexOrder

instance IsSubface Edge OTriangle where
    isSubface = (. forgetVertexOrder) . isSubface

instance IsSubface OEdge OTriangle where
    isSubface = (. forgetVertexOrder) . isSubface






-- instance Show TIndex where
--     show (TIndex x) = "\916"++show x














instance IsSubface IVertex TIndex where isSubface x i = getTIndex x == i 
instance IsSubface IEdge TIndex where isSubface x i = getTIndex x == i 
instance IsSubface OIEdge TIndex where isSubface x i = getTIndex x == i 
instance IsSubface ITriangle TIndex where isSubface x i = getTIndex x == i 
instance IsSubface OITriangle TIndex where isSubface x i = getTIndex x == i 

-- $(concat `liftM` 
--     mapM  (\t -> 
--         [d| 
--             instance IsSubface $(t) TIndex where isSubface x i = $(varE '(==)) (getTIndex x) i 
--           |])
--     [conT ''IVertex, conT ''IEdge, conT ''ITriangle] 
--  )


    
instance (IsSubface a b) => IsSubface (I a) (I b) where
    isSubface (I i a) (I j b) = i == j && isSubface a b

liftIsSubface
  :: (HasTIndex ia a, HasTIndex ia1 a1, IsSubface a a1) =>
     ia -> ia1 -> Bool
liftIsSubface x y = isSubface (viewI x) (viewI y)

instance IsSubface IVertex IEdge where isSubface = liftIsSubface
instance IsSubface IVertex ITriangle where isSubface = liftIsSubface
instance IsSubface IEdge ITriangle where isSubface = liftIsSubface
instance IsSubface OIEdge ITriangle where isSubface = liftIsSubface


sumIsSubface
  :: (SubSumTy b, IsSubface (L b) b1, IsSubface (R b) b1) =>
     b -> b1 -> Bool
sumIsSubface = flip (\b -> flip isSubface b |||| flip isSubface b)

-- | = 'sumIsSubface'
instance (IsSubface v x, IsSubface e x) => IsSubface (AnySimplex1 v e) x where
    isSubface = sumIsSubface

-- | = 'sumIsSubface'
instance (IsSubface v x, IsSubface e x, IsSubface t x) => IsSubface (AnySimplex2 v e t) x where
    isSubface = sumIsSubface


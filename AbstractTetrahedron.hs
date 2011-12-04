{-# LANGUAGE CPP, NoMonomorphismRestriction, ImplicitParams, TypeFamilies, TypeOperators, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances, ViewPatterns, BangPatterns #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall #-}
module AbstractTetrahedron(
    module S2,
    module S3,
    module Util,
    module TIndex,
    module Data.Monoid,

    -- * Vertices
    Vertex,
    vA,vB,vC,vD,
    allVertices,allVertices',
    MakeVertex(..),
    Vertices(..),
    vertexList,
    IVertex,
    -- * Edges
    Edge, eAB,eAC,eAD,eBC,eBD,eCD,
    allEdges,allEdges',
    MakeEdge(..),
    Edges(..),
    edgeList,
    oppositeEdge,
    IEdge,
    -- * Triangles
    module Triangle,Triangles(..),triangleList,
    -- * Ordered faces
    OrderableFace(..), defaultLeftActionForOrderedFace,
    forgetVertexOrder,
    IsSubface(..),liftIsSubface,
    -- ** Ordered edges
    OEdge,
    verticesToOEdge,
    OEdges(..),
    MakeOEdge(..),
    OIEdge,


    -- * Testing
    qc_AbstractTetrahedron)
    
    where

import Control.Exception
import Control.Monad.RWS
import HomogenousTuples
import Prelude hiding(catch,lookup)
import S2
import S3
import Test.QuickCheck
import Test.QuickCheck.All
import Util
import Vertex
import Edge
import Triangle
import TIndex
import FaceClasses
import OrderableFace
import Data.Monoid

class IsSubface x y where
    isSubface :: x -> y -> Bool

instance IsSubface Vertex Edge where
    isSubface = isVertexOfEdge

instance IsSubface Vertex Triangle where
    isSubface = isVertexOfTriangle

instance IsSubface Edge Triangle where
    isSubface = isEdgeOfTriangle

prop_IsSubface_transitive :: Vertex -> Edge -> Triangle -> Property
prop_IsSubface_transitive v e f = (isSubface v e && isSubface e f) ==> isSubface v e  

prop_IsSubface_count_VE :: Vertex -> Bool
prop_IsSubface_count_VE v = length ( filter6 (v `isSubface`) allEdges' ) == 3

prop_IsSubface_count_VF :: Vertex -> Bool
prop_IsSubface_count_VF v = length ( filter4 (v `isSubface`) allTriangles' ) == 3

prop_IsSubface_count_EF :: Edge -> Bool
prop_IsSubface_count_EF e = length ( filter4 (e `isSubface`) allTriangles' ) == 2

prop_edgeByOppositeVertexAndTriangle :: Vertex -> Triangle -> Property
prop_edgeByOppositeVertexAndTriangle v t | isSubface v t = (isSubface e t .&. not (isSubface v e)) 
                                         | otherwise = expectFailure (seq e True)
    where
        e = edgeByOppositeVertexAndTriangle v t


prop_edgesContainingVertex :: Vertex -> Bool
prop_edgesContainingVertex v = all3 (isSubface v) (edges v)

prop_trianglesContainingVertex :: Vertex -> Bool
prop_trianglesContainingVertex v = all3 (isSubface v) (triangles v)

qc_AbstractTetrahedron ::  IO Bool
qc_AbstractTetrahedron = $(quickCheckAll)

class Intersection a b c | a b -> c where
    intersect :: a -> b -> c

instance Intersection Edge Edge (Maybe (Either Edge Vertex)) where
    intersect = intersectEdges 

instance Intersection Triangle Triangle (Either Triangle Edge) where
    intersect t1 t2 = case filter3 (\v -> elem3 v (edges t2)) (edges t1) of
                           [e] -> assert (t1/=t2) (Right e)
                           [_,_,_] -> assert (t1==t2) (Left t1)
                           _ -> assert False (error "impossible")














prop_VerticesToOTriangle ::  Vertex -> Property
prop_VerticesToOTriangle v0 =
    forAll (arbitrary `suchThat` (/= v0)) $ \v1 ->
    forAll (arbitrary `suchThat` (liftM2 (&&) (/= v0) (/= v1))) $ \v2 ->
        
        let vs = (v0,v1,v2) in vs .=. vertices (verticesToOTriangle vs) 


prop_VerticesToOEdge ::  Vertex -> Property
prop_VerticesToOEdge v0 =
    forAll (arbitrary `suchThat` (/= v0)) $ \v1 ->
        let vs = (v0,v1) in vs .=. vertices (verticesToOEdge vs) 



prop_MakeEdge :: (Vertex,Vertex) -> Property
prop_MakeEdge vs@(v0,v1) = v0 < v1 ==> (vertices (edge vs) == vs)
                            









-- instance Enum OEdge where
--     toEnum n = case toEnum n of EnumPair a b -> OEdge a b
--     fromEnum (OEdge a b) = fromEnum (EnumPair a b)





instance (IsSubface a b) => IsSubface (I a) (I b) where
    isSubface (I i a) (I j b) = i == j && isSubface a b




-- instance Show TIndex where
--     show (TIndex x) = "\916"++show x














#define F(T) instance IsSubface T TIndex where isSubface x i = getTIndex x == i 
F(IVertex)
F(IEdge)
F(ITriangle)
#undef F

-- $(concat `liftM` 
--     mapM  (\t -> 
--         [d| 
--             instance IsSubface $(t) TIndex where isSubface x i = $(varE '(==)) (getTIndex x) i 
--           |])
--     [conT ''IVertex, conT ''IEdge, conT ''ITriangle] 
--  )


    

liftIsSubface
  :: (HasTIndex ia a, HasTIndex ia1 a1, IsSubface a a1) =>
     ia -> ia1 -> Bool
liftIsSubface x y = isSubface (viewI x) (viewI y)

instance IsSubface IVertex IEdge where isSubface = liftIsSubface
instance IsSubface IVertex ITriangle where isSubface = liftIsSubface
instance IsSubface IEdge ITriangle where isSubface = liftIsSubface

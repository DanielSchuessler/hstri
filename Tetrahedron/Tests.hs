{-# LANGUAGE TemplateHaskell #-}
module Tetrahedron.Tests where

import AbstractTetrahedron
import Test.QuickCheck
import Test.QuickCheck.All
import QuickCheckUtil
import Element
import Data.List as L
import Data.Proxy


qc_Tetrahedron_Tests = $quickCheckAll

-- * Vertex

prop_viewUnviewVertex :: Vertex -> Property
prop_viewUnviewVertex v = v .=. unviewVertex (viewVertex v) 

prop_toFromWord8 :: Vertex -> Property
prop_toFromWord8 v = v .=. vertexFromWord8 (vertexToWord8 v) 

prop_otherVertices :: Vertex -> Bool
prop_otherVertices v = 
    asList (otherVertices v) == sort (filter (/= v) allVertices) 


-- * Edge

prop_OrderableFace_Edge :: Property
prop_OrderableFace_Edge = polyprop_OrderableFace (undefined :: Proxy Edge)

prop_EnumEdge :: Property
prop_EnumEdge = forAll (elements [0..5]) (\n -> fromEnum (toEnum n :: Edge) .=. n)

prop_OppositeEdge_Order2 ::  Edge -> Property
prop_OppositeEdge_Order2 e = let e' = oppositeEdge e in (e' /= e) .&. (oppositeEdge e' == e)



prop_OppositeEdge_disjoint ::  Edge -> Property
prop_OppositeEdge_disjoint e = L.intersect (edgeVertexList e) (edgeVertexList (oppositeEdge e)) .=. [] 
    where
        edgeVertexList = asList . edgeVertices
prop_EnumOEdge :: Property
prop_EnumOEdge = forAll (elements [0..11]) (\n -> fromEnum (toEnum n :: OEdge) .=. n)

prop_OrderableFace_IEdge :: Property
prop_OrderableFace_IEdge = polyprop_OrderableFace (undefined :: Proxy IEdge)
prop_joinIVertexAndEdge :: ITriangle -> Property
prop_joinIVertexAndEdge t = 
    forAllElements (vertexList t)
        (\v ->
            t .=. joinIVertexAndEdge v (edgeByOppositeVertexAndTriangle (unI v) (unI t)))
prop_verticesOfTriangle :: Triangle -> Property
prop_verticesOfTriangle t =
    asList (verticesOfTriangle t)  
    .=.
    sort (filter (`isVertexOfTriangle` t) allVertices)
prop_MakeTriangle_VVV ::  Vertex -> Property
prop_MakeTriangle_VVV v0 = 
    forAll (elements vs') $ 
        \v1 -> forAll (elements (vs' \\ [v1])) $ 
            \v2 -> 
                let v012 = (v0,v1,v2) in asList v012 `setEq` (asList . verticesOfTriangle) (triangle v012)
    where
        vs' = allVertices \\ [v0]

prop_MakeTriangle_EE ::  Edge -> Edge -> Property
prop_MakeTriangle_EE e1 e2 = (e1 /= e2 && e1 /= oppositeEdge e2) ==> 
                                let
                                   t = triangle (e1,e2)
                                in
                                    (e1 `isEdgeOfTriangle` t) .&. (e2 `isEdgeOfTriangle` t)
                                    
prop_OrderableFace_ITriangle :: Property
prop_OrderableFace_ITriangle = polyprop_OrderableFace (undefined :: Proxy ITriangle)

prop_OrderableFace_Triangle :: Property
prop_OrderableFace_Triangle = polyprop_OrderableFace (undefined :: Proxy Triangle)

prop_trianglesContainingVertex :: Vertex -> Property
prop_trianglesContainingVertex v =
   setEq 
    (asList (trianglesContainingVertex v))  
    (filter (isVertexOfTriangle v) allTriangles)


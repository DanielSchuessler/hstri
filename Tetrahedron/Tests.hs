{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TemplateHaskell #-}
module Tetrahedron.Tests where

import Tetrahedron
import Test.QuickCheck
import Test.QuickCheck.All
import QuickCheckUtil
import Element
import Data.List as L
import Data.Proxy
import HomogenousTuples
import Control.Monad
import Tetrahedron.NormalDisc
import Data.Ix


qc_Tetrahedron = $quickCheckAll

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
prop_edgesContainingVertex v = all3 (isSubface v) (star v (OneSkeleton AbsTet))

prop_starVertexInTwoSkel :: Vertex -> Bool
prop_starVertexInTwoSkel v = all3 (isSubface v) (star v (TwoSkeleton AbsTet))

prop_VerticesToOTriangle ::  Vertex -> Property
prop_VerticesToOTriangle v0 =
    forAll (arbitrary `suchThat` (/= v0)) $ \v1 ->
    forAll (arbitrary `suchThat` (liftM2 (&&) (/= v0) (/= v1))) $ \v2 ->
        
        let vs = (v0,v1,v2) in vs .=. vertices (oTriangleByVertices vs) 


prop_VerticesToOEdge ::  Vertex -> Property
prop_VerticesToOEdge v0 =
    forAll (arbitrary `suchThat` (/= v0)) $ \v1 ->
        let vs = (v0,v1) in vs .=. vertices (verticesToOEdge vs) 



prop_MakeEdge :: (Vertex,Vertex) -> Property
prop_MakeEdge vs@(v0,v1) = v0 < v1 ==> (vertices (edge vs) == vs)

prop_Triangle_NormalArcs_correct :: Triangle -> Bool
prop_Triangle_NormalArcs_correct t = all3 (`isSubface` t) (normalArcs t) 

prop_Triangle_NormalArcs_complete :: NormalArc -> Triangle -> Property
prop_Triangle_NormalArcs_complete nat t = 
    isSubface nat t ==> 
        any3 (==nat) (normalArcs t) 

prop_normalArcByNormalCorners :: NormalArc -> Property
prop_normalArcByNormalCorners na = 
        na == normalArc (nc1,nc2)
        .&.
        na == normalArc (nc2,nc1)
    where
        (nc1,nc2) = normalCorners na 

prop_normalArcGetAngle :: NormalArc -> Property
prop_normalArcGetAngle na =
        v .=. normalArcGetVertex na
        .&.
        triangle vs .=. normalArcGetTriangle na

    where
        vs@(_,v,_) = normalArcGetAngle na

prop_normalArcGetAngle_corners :: NormalArc -> Property
prop_normalArcGetAngle_corners na =
        na .=. normalArc (nc0,nc1)

    where
        (v0,v,v1) = normalArcGetAngle na
        nc0 = normalCorner (v0,v) 
        nc1 = normalCorner (v1,v) 

prop_NormalCornersOfNormalArc_distinct :: NormalArc -> Bool
prop_NormalCornersOfNormalArc_distinct nat = let (c1,c2) = normalCorners nat in c1 /= c2

prop_normalArcsAroundVertex :: Vertex -> Property
prop_normalArcsAroundVertex v =
    setEq
        (asList . normalArcsAroundVertex $ v)
        (filter ((==v) . normalArcGetVertex) allNormalArcs)

prop_normalQuadGetIntersectedEdges ::  NormalQuad -> Bool
prop_normalQuadGetIntersectedEdges nqt = 
    sort allEdges == sort (toList4 (normalQuadGetIntersectedEdges nqt) 
        ++ asList (normalQuadGetDisjointEdges nqt))


prop_NormalDisc_NormalArcs_correct :: NormalDisc -> Bool
prop_NormalDisc_NormalArcs_correct ndt = all (`isSubface` ndt) (normalArcs ndt) 

prop_NormalDisc_NormalArcs_complete :: NormalArc -> NormalDisc -> Property
prop_NormalDisc_NormalArcs_complete nat ndt = 
    isSubface nat ndt ==> 
        any (==nat) (normalArcs ndt) 

prop_normalQuadByNormalArc :: NormalArc -> Bool
prop_normalQuadByNormalArc na = isSubface na (normalQuadByNormalArc na) 

prop_normalTriByNormalArc :: NormalArc -> Bool
prop_normalTriByNormalArc na = isSubface na (normalTriByNormalArc na) 

prop_link_nc_nq :: NormalCorner -> NormalQuad -> Property
prop_link_nc_nq nc nq =
    isSubface nc nq ==>
        let
            (nc0,nc1) = link nc nq 
        in
            conjoin' [
                isSubface nc0 nq,
                isSubface nc1 nq,
                nc0 /= nc,
                nc1 /= nc,
                nc0 /= nc1
            ]
        
-- | Laws from the 'Ix' documentation
prop_Ix_NormalDisc :: NormalDisc -> NormalDisc -> Property
prop_Ix_NormalDisc (l :: NormalDisc) u =
    (\i -> inRange (l,u) i == elem i (range (l,u)))
    .&&.
    (\i -> inRange (l,u) i ==> range (l,u) !! index (l,u) i == i)
    .&&.
    (map (index (l,u)) (range (l,u)) == [0..rangeSize (l,u)-1])
    .&&.
    (rangeSize (l,u) == length (range (l,u)))


polyprop_faceMapConsistency2 t =
    case edges t of
         dits ->

             printTestCase (show dits) $

                 case map3 vertices dits of

                     ( (d0d0,d1d0)
                      ,(d0d1,d1d1)
                      ,(d0d2,d1d2))

                      ->
                          printTestCase ("Checking d0 . d0 = d0 . d1")
                            (d0d0 .=. d0d1)

                          .&&.
                          
                          printTestCase ("Checking d1 . d0 = d0 . d2")
                            (d1d0 .=. d0d2)

                          .&&.

                          printTestCase ("Checking d1 . d1 = d1 . d2")
                            (d1d1 .=. d1d2)



polyprop_faceMapConsistency3
  :: (Eq e,
      Show tri,
      Show e,
      Triangles tet,
      Edges tri,
      Tris tet ~ (tri, tri, tri, tri),
      Eds tri ~ (e, e, e)) =>
     tet -> Property
polyprop_faceMapConsistency3 t =
    case triangles t of
         dits ->

             printTestCase (show dits) $

                 case map4 edges dits of

                     ( (d0d0,d1d0,d2d0)
                      ,(d0d1,d1d1,d2d1)
                      ,(d0d2,d1d2,d2d2)
                      ,(d0d3,d1d3,d2d3)
                      )

                      ->
                          printTestCase ("Checking d0 . d0 = d0 . d1")
                            (d0d0 .=. d0d1)

                          .&&.
                          
                          printTestCase ("Checking d1 . d0 = d0 . d2")
                            (d1d0 .=. d0d2)

                          .&&.

                          printTestCase ("Checking d1 . d1 = d1 . d2")
                            (d1d1 .=. d1d2)

                          .&&.
                          
                          printTestCase ("Checking d2 . d0 = d0 . d3")
                            (d2d0 .=. d0d3)

                          .&&.

                          printTestCase ("Checking d2 . d1 = d1 . d3")
                            (d2d1 .=. d1d3)

                          .&&.

                          printTestCase ("Checking d2 . d2 = d2 . d3")
                            (d2d2 .=. d2d3)


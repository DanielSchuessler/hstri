{-# LANGUAGE ScopedTypeVariables, ViewPatterns, TupleSections, TemplateHaskell #-}
module Triangulation.Tests where

import Triangulation
import Triangulation.InnerNormalArc
import Triangulation.CanonOrdered
import TriangulationCxtObject
import Triangulation.Random
import Test.QuickCheck
import Control.Applicative
import Tetrahedron.Tests
import THUtil
import Control.Monad.State
import Test.QuickCheck.All
import QuickCheckUtil
import Data.Proxy
import Data.EdgeLabelledTree
import HomogenousTuples
import Triangulation.Transformations
import Equivalence.Tests
import Data.Function
import Data.SumType
import Quote
import Simplicial.DeltaSet3
import Triangulation.AbstractNeighborhood
import Triangulation.PreTriangulation(lookupO)
import Text.Groom



qc_Triangulation ::  IO Bool
qc_Triangulation = $quickCheckAll

forAllNonEmptyIntTriangulations :: Testable prop => (Triangulation -> prop) -> Property
forAllNonEmptyIntTriangulations f = property (\t ->
    not (Prelude.null (tTetrahedra_ t)) ==>
        (f t)  )

prop_tGluingsIrredundant :: Triangulation -> Bool
prop_tGluingsIrredundant tr = 
    tGlueMap_ tr == tGlueMap_ (mkTriangulation
                        (tNumberOfTetrahedra tr) 
                        (tGluingsIrredundant tr)) 

prop_getOIEdgeGluingSense_same :: Triangulation -> Property
prop_getOIEdgeGluingSense_same tr = forAllElements (tOIEdges tr)
    (\e -> getOIEdgeGluingSense tr e e == Just NoFlip )
    
prop_getOIEdgeGluingSense_sym :: Triangulation -> Property
prop_getOIEdgeGluingSense_sym tr = 
    join forAllElements2 (tOIEdges tr) (\(e1, e2) -> f e1 e2 == f e2 e1)
  where
        f = getOIEdgeGluingSense tr

prop_forgetVertexOrder_natural_for_canonicalization :: Triangulation -> Property
prop_forgetVertexOrder_natural_for_canonicalization t =
    forAllElements (tOIEdges t)
        (\e -> canonicalize t (forgetVertexOrder e) == forgetVertexOrder (canonicalize t e)) 

prop_iEdgeEqv :: Triangulation -> Property
prop_iEdgeEqv tr = polyprop_Equivalence' (iEdgeEqv tr) (tIEdges tr) 

prop_CanonicallyOrderable_Triangle :: Triangulation -> Property
prop_CanonicallyOrderable_Triangle = polyprop_CanonicallyOrderable (undefined :: Proxy OITriangle)

prop_CanonicallyOrderable_Edge :: Triangulation -> Property
prop_CanonicallyOrderable_Edge = polyprop_CanonicallyOrderable (undefined :: Proxy OIEdge)

prop_vertexEquivalence :: Triangulation -> Property
prop_vertexEquivalence tr = polyprop_Equivalence' (vertexEquivalence tr) (tIVertices tr)

prop_edgeEquivalence :: Triangulation -> Property
prop_edgeEquivalence tr = polyprop_Equivalence' (edgeEquivalence tr) (tCOIEdges tr)
                                      
prop_triangleEquivalence :: Triangulation -> Property
prop_triangleEquivalence tr = polyprop_Equivalence' (triangleEquivalence tr) (tCOITriangles tr)

prop_iTriangleEqv :: Triangulation -> Property
prop_iTriangleEqv tr = polyprop_Equivalence' (iTriangleEqv tr) (tITriangles tr)

prop_VerticesOfEdge_welldefined :: Triangulation -> Property
prop_VerticesOfEdge_welldefined tr = 
    polyprop_respects (iEdgeEqv tr) trivialEquivalence 
       (elements (tIEdges tr))
       (sort2 . tVerticesOfIEdge tr) 


prop_IsSubface_TTet_welldefined :: Triangulation -> Property
prop_IsSubface_TTet_welldefined t = 
    forAllElements2 (unT <$> triangles t) (tTetrahedra_ t)
        (mkWellDefinednessProp2 (eqvTriangles t) ((:[]))
            (isSubface_TTet t))  

prop_IsSubface_VE_welldefined :: Triangulation -> Property
prop_IsSubface_VE_welldefined t = 
    forAllElements (vertices t) $ \v ->
    polyprop_respects (iEdgeEqv t) trivialEquivalence (elements $ tIEdges t)
        (isSubface_VE v)



prop_IsSubface_ET_welldefined :: Triangulation -> Property
prop_IsSubface_ET_welldefined t = 
    forAllElements (edges t) $ \e ->
        polyprop_respects 
            (iTriangleEqv t) 
            trivialEquivalence 
            (elements $ tITriangles t)
            (isSubface_ET e)

prop_TIndexToTTriangles_surjective ::  Triangulation -> Property
prop_TIndexToTTriangles_surjective t = setEq (triangles t) (concatMap (triangleList . (t,)) (tTetrahedra_ t)) 

prop_TTrianglesToTEdges_surjective ::  Triangulation -> Property
prop_TTrianglesToTEdges_surjective t = setEq (edges t) (concatMap edgeList (triangles t)) 


prop_TEdgesToTVertices_surjective ::  Triangulation -> Property
prop_TEdgesToTVertices_surjective t = setEq (vertices t) (concatMap vertexList (edges t)) 

prop_normalArcsOfTriangulationAreCanonical :: Triangulation -> Property
prop_normalArcsOfTriangulationAreCanonical tr =
    forAll (elements (normalArcs tr))
        (\(unT -> arc) -> arc == canonicalize tr arc)

prop_normalArcsOfTriangulationAreDistinct :: Triangulation -> Property
prop_normalArcsOfTriangulationAreDistinct tr =
    noDupes (normalArcs tr :: [TNormalArc])

prop_normalArcs_triangles :: Triangulation -> Property
prop_normalArcs_triangles tr =
    length (innerNormalArcs tr) .=. 3 * length (innerTriangles tr) 
    .&.
    length (boundaryNormalArcs tr) .=. 3 * length (boundaryTriangles tr) 

prop_dfsVertexLink :: Triangulation -> Property
prop_dfsVertexLink tr = forAllElements (vertices tr)
    (\v -> case dfsVertexLink v of
                elt -> $(ppTestCase 'elt) 
                
                        ((noDupes . eltNodes) elt
                         .&.
                         setEq (eltNodes elt) (vertexLinkingSurfaceTris v)
                         .&.
                         isSubset 
                            (map (uncurry innNAFromPreimage) . eltEdges $ elt) 
                            (innNAsAroundVertex $ v)
                        
                        ))

prop_normalizedGluings_noDupes :: Triangulation -> Property
prop_normalizedGluings_noDupes = noDupes . tNormalizedGluings

prop_deleteTwoTets :: Triangulation -> Property
prop_deleteTwoTets tr =
    let 
        n = tNumberOfTetrahedra tr
    in
        n >= 2 ==>

        forAll (choose (0,n-2)) (\i -> 
            forAll (choose (i+1,n-1)) (\j ->

                ((.=.) `on` sumTypeToMaybe) 
                    ((deleteTet' i <=< deleteTet' j) tr)
                    ((deleteTet' (j-1) <=< deleteTet' i) tr)))

                    
prop_normalizeGluing :: Property
prop_normalizeGluing = forAll gluingGen (isGluingNormalized . ngToGluing . normalizeGluing)

prop_gluingMapVertex :: Gluing -> Property
prop_gluingMapVertex gl =
    glCod gl .=. oiTriangleByVertices us
  where
    us = map3 (gluingMap gl) (vertices (glDom gl))


prop_gluingMapOITriangle :: OITriangle -> OITriangle -> Property
prop_gluingMapOITriangle ot1 ot2 =
    gluingMap (oiTriangleGluing ot1 ot2) ot1 .=. ot2


prop_closedTriangulationGenGeneratesClosed
  :: ClosedTriangulation -> Bool
prop_closedTriangulationGenGeneratesClosed (ClosedTriangulation tr) =
    isClosedTriangulation tr


prop_edgeNeighborhood (ManifoldTriangulation tr) =
    forAllElements (tOIEdges tr)
        (\e ->
            let
                en = (edgeNeighborhood tr e)
            in
                printTestCase ("en = "++groom en) $
                        either (ben_valid tr e) (ien_valid tr e) en) 

ben_valid :: Triangulation -> OIEdge -> BoundaryEdgeNeighborhood -> Property
ben_valid tr e (ben_toList -> ients) = 
    en_valid_common tr e ients
    .&&.
    let
        b = isBoundaryTriangle . pMap tr . forgetVertexOrder
    in
        b (ient_leftTri (head ients))
        .&&.
        b (ient_rightTri (last ients))

ien_valid :: Triangulation -> OIEdge -> InnerEdgeNeighborhood -> Property
ien_valid tr e (ien_toList -> ients) = 
    en_valid_common tr e ients
    .&&.
    lookupO (ient_leftTri (head ients)) tr .=. Just (ient_rightTri (last ients))

en_valid_common
  :: Triangulation -> OIEdge -> [IEdgeNeighborhoodTet] -> Property
en_valid_common tr e ients =
    printTestCase "en_valid_common1"
    (conjoin [ pMap tr e .=. pMap tr (oiEdgeByVertices (ient_bot ient, ient_top ient)) 
                    | ient <- ients ])
    .&&.
    printTestCase "en_valid_common2"
    (
    conjoin
    [
    lookupO (ient_rightTri ient0) tr .=. Just (ient_leftTri ient1)
    |
        (ient0,ient1) <- zip ients (tail ients)
    ]
    )


prop_ient
  :: TIndex -> Vertex -> Vertex -> Vertex -> Vertex -> Property
prop_ient i a b c d =
    let
        go f v = f (i ./ unsafeEdgeNeighborhoodTetExportedOnlyForTesting a b c d) .=. i ./ v
    in
        conjoin [
            go ient_top a,
            go ient_bot b,
            go ient_left c,
            go ient_right d
        ]



-- prop_SatisfiesSimplicialIdentities2_TTriangle
--   :: TTriangle -> Property
-- prop_SatisfiesSimplicialIdentities2_TTriangle (t :: TTriangle) =
--     printTestCase (quote t) $
--     polyprop_SatisfiesSimplicialIdentities2 t
--     
-- tr_ = mkTriangulation 1 [(0 ./ tBCD, 0 ./ oBCA), (0 ./ tACD, 0 ./ oABD)]
-- t = pMap tr_ (0 ./ tACD)
-- 
-- (d0t,d1t,d2t) = edges t
-- 

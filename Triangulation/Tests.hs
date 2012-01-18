{-# LANGUAGE ViewPatterns, TupleSections, TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module Triangulation.Tests where

import Triangulation
import Triangulation.CanonOrdered
import TriangulationCxtObject
import Triangulation.Random()
import Test.QuickCheck
import Control.Applicative
import THUtil
import Control.Monad.State
import Test.QuickCheck.All
import QuickCheckUtil
import Data.Proxy
import Data.EdgeLabelledTree
import HomogenousTuples



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

-- prop_tNormalArcsAroundVertex tr =
--     forAllElements (vertices tr) $ \v ->
--         setEq
--             (tNormalArcsAroundVertex v)
--             (filter 



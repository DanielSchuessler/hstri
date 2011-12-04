{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ViewPatterns, TypeOperators, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module EdgeCentered(
    module Triangulation,
    module SimplicialPartialQuotient,
    EdgeNeighborhoodVertex(..),
    makeEdgeNeighborhoodMap,
    makeEdgeNeighborhood,
    prop_edgeIsGlued,

    ) where

import Equivalence
import Triangulation
import Data.Map as M
import Data.List as L
import HomogenousTuples
import Test.QuickCheck
import Simplicial.SimplicialComplex
import Data.Vect.Double hiding((.*))
import Data.Maybe
import Control.Monad
import PreRenderable
import SimplicialPartialQuotient

data EdgeNeighborhoodVertex =
    Bottom |
    Top |
    RingVertex Int

    deriving(Eq,Ord)

instance Show EdgeNeighborhoodVertex where
    show Bottom = "B"
    show Top = "T"
    show (RingVertex i) = show i


makeEdgeNeighborhoodMap :: Triangulation -> OIEdge -> SimplicialPartialQuotient EdgeNeighborhoodVertex
makeEdgeNeighborhoodMap tr oiEdge =
    let               
        oEdgeClass = tOIEdgeEquivalents tr oiEdge

        n = length oEdgeClass
        
        initialTet = getTIndex oiEdge

        (initialv0,initialv1) = vertices (forgetTIndex oiEdge)

        -- Arbitrary choice of orientation; these could be flipped
        [initialv2,initialv3] = allVertices L.\\ [initialv0,initialv1] 

        makeMapFragmentForTet tet i bottomPreimage topPreimage ringVertexPreimage ringVertexPreimage'
            = M.fromList
                        [ ( tet ./ bottomPreimage, Bottom)
                        , ( tet ./ topPreimage, Top)
                        , ( tet ./ ringVertexPreimage, RingVertex i)
                        , ( tet ./ ringVertexPreimage', RingVertex (mod (i+1) n))
                        ] 

        initialMap = makeMapFragmentForTet initialTet 0 initialv0 initialv1 initialv2 initialv3

        loop 
            i 
            m 
            prevTet
            prevBottomPreimage
            prevTopPreimage
            prevRingVertexPreimage
             
             | i == n = m

             | otherwise =
                let
                    prevTriangle :: OITriangle
                    prevTriangle = prevTet ./ otriangle (prevBottomPreimage
                                                           ,prevTopPreimage
                                                           ,prevRingVertexPreimage)
                    
                    tri = case lookupGluingOfOITriangle tr prevTriangle of
                               Just x -> x
                               Nothing -> error ("Triangle "++show prevTriangle++
                                                 " is a boundary triangle; expected a loop of gluings")

                    currentTet = getTIndex tri

                    vs@(v0,v1,v2) = vertices (forgetTIndex tri)
                    
                    [v3] = allVertices L.\\ toList3 vs 

                    m' = M.unionWith collision m 
                            (makeMapFragmentForTet currentTet i v0 v1 v2 v3) 

                    collision _ _ = error (
                        "Tet "++show currentTet++" already encountered; this function only works"
                        ++" on an edge which only has one preimage in each tetrahedron. The equivalence"
                        ++" class of the edge is:\n" ++ show oEdgeClass
                        ++"\nThe mapping so far is:\n"
                        ++(unlines . fmap show . M.toList) m)
                    
                in
                    --trace (concat ["i=",show i,"; prevTriangle=",show prevTriangle,"; tri=",show tri]) $
                    
                    loop (i+1) m' currentTet v0 v1 v3


        tets = ([ (Bottom,Top,RingVertex i,RingVertex (i+1))
                                    
                        | i <- [0..n-2] ]

                        ++ [(Bottom,Top,RingVertex 0,RingVertex (n-1))])


        res = loop 1 initialMap initialTet initialv0 initialv1 initialv3  
    in
        fromMap tr res tets 



makeEdgeNeighborhood :: Triangulation -> OIEdge -> 
    (SimplicialPartialQuotient EdgeNeighborhoodVertex, 
     PreRenderable (OTuple EdgeNeighborhoodVertex))

makeEdgeNeighborhood tr oiEdge =
    let

        m = makeEdgeNeighborhoodMap tr oiEdge

        n = tOIEdgeDegree tr oiEdge



        h = 0.5


        coords Bottom = (-h) *& vec3Z
        coords Top = h *& vec3Z
        coords (RingVertex i) = Vec3 (sin phi) (-(cos phi)) 0  
            where 
                phi = 2*pi*(2*i'-1)/(2*n')
                i' = fromIntegral i
                n' = fromIntegral n

        

--         labels = fmap return letters ++ join (liftM2 (\x y -> [x,y])) letters ++ error "out of labels :("
--         letters = "FGJPQR?" 

            
    in
        (m, toPreRenderable m coords) 



prop_edgeIsGlued
  :: Eq b => SimplicialPartialQuotient b -> OIEdge -> Property
prop_edgeIsGlued m oiedge = 
    let
        oEdgeClass = eqv_equivalents (oEdgeEqv . spq_tr $ m) oiedge
        im_oiedge = mapEdge m oiedge
    in
        forAll (elements oEdgeClass)
            (\oiedge2 -> mapEdge m oiedge2 == im_oiedge)
        


        
--qc_EdgeCentered = $(quickCheckAll)

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
import Data.Vect.Double hiding((.*))
import Data.Maybe
import Control.Monad
import SimplicialPartialQuotient
import PrettyUtil
import ShortShow

data EdgeNeighborhoodVertex =
    Bottom |
    Top |
    EquatorVertex Int

    deriving(Show,Eq,Ord)

instance ShortShow EdgeNeighborhoodVertex where
    shortShow Bottom = "B"
    shortShow Top = "T"
    shortShow (EquatorVertex i) = show i

instance Pretty EdgeNeighborhoodVertex where
    pretty = black . text . show


makeEdgeNeighborhoodMap :: Triangulation -> OIEdge -> SimplicialPartialQuotient EdgeNeighborhoodVertex
makeEdgeNeighborhoodMap tr oiEdge =
    let               
        oEdgeClass = tOIEdgeEquivalents tr oiEdge

        n = length oEdgeClass
        
        initialTet = getTIndex oiEdge

        (initialv0,initialv1) = vertices (forgetTIndex oiEdge)

        -- Arbitrary choice of orientation; these could be flipped
        [initialv2,initialv3] = allVertices L.\\ [initialv0,initialv1] 

        makeMapFragmentForTet tet i bottomPreimage topPreimage 
                                equatorVertexPreimage equatorVertexPreimage'
            = M.fromList
                        [ ( tet ./ bottomPreimage, Bottom)
                        , ( tet ./ topPreimage, Top)
                        , ( tet ./ equatorVertexPreimage, EquatorVertex i)
                        , ( tet ./ equatorVertexPreimage', EquatorVertex (mod (i+1) n))
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


        tets = ([ (Bottom,Top,EquatorVertex i,EquatorVertex (i+1))
                                    
                        | i <- [0..n-2] ]

                        ++ [(Bottom,Top,EquatorVertex 0,EquatorVertex (n-1))])


        res = loop 1 initialMap initialTet initialv0 initialv1 initialv3  
    in
        fromMap tr res tets 




makeEdgeNeighborhood
  :: Triangulation
     -> OIEdge
     -> (Gluing -> String)
     -> SPQWithCoords EdgeNeighborhoodVertex
makeEdgeNeighborhood tr oiEdge gluingLabeller =
    let

        m = makeEdgeNeighborhoodMap tr oiEdge

        n = tOIEdgeDegree tr oiEdge



        h = max 0.2 (sqrt (equatorEdgeLengthSqr - 1))

        equatorEdgeLengthSqr = normsqr (coords (EquatorVertex 1) &- coords (EquatorVertex 0))

        -- equatorEdgeLength = sqrt (1 + h^2)
        -- sqrt (equatorEdgeLengthSqr - 1) = h




        coords Bottom = (-h) *& vec3Z
        coords Top = h *& vec3Z
        coords (EquatorVertex i) = Vec3 (sin phi) (-(cos phi)) 0  
            where 
                phi = 2*pi*(2*i'-1)/(2*n')
                i' = fromIntegral i
                n' = fromIntegral n

        

--         labels = fmap return letters ++ join (liftM2 (\x y -> [x,y])) letters ++ error "out of labels :("
--         letters = "FGJPQR?" 

    in    
        SPQWithCoords m coords gluingLabeller
            


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

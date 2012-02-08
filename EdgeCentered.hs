{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, NoMonomorphismRestriction, ViewPatterns, TypeOperators, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module EdgeCentered(
    module Triangulation,
    module SimplicialPartialQuotient,
    EdgeNeighborhoodVertex(..),
    makeEdgeNeighborhood,
    polyprop_edgeIsGlued,

    ) where

import Equivalence
import Triangulation
import Data.Map as M
import Data.List as L
import HomogenousTuples
import Test.QuickCheck
import Data.Vect.Double hiding((.*))
import Data.Maybe
import SimplicialPartialQuotient
import PrettyUtil
import ShortShow
import DisjointUnion
import AbstractNeighborhood

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
 case innerEdgeNeighborhood' tr oiEdge of
  Nothing -> error ("makeEdgeNeighborhoodMap: This function only works on inner edges, not "
                                ++ show oiEdge)

  Just tets ->
    let               
        n = length tets
        
        theAssocs = zipWith 
                        (\i ent -> 
                            ( ( mapI ent_bot ent   , Bottom)
                            , ( mapI ent_top ent   , Top)
                            , ( mapI ent_left ent  , EquatorVertex i)
                            , ( mapI ent_right ent , EquatorVertex (mod (i+1) n))
                            ) 
                        )
                        [0..]
                        tets

        res = M.fromListWith collision (concatMap toList4 theAssocs)

            where
                collision _ _ = error (
                    prettyString (
                        vsep [
                            text (
                               "makeEdgeNeighborhoodMap: This function only works"
                            ++ " on an edge which only has one preimage in each tetrahedron."
                            ++ " The preimage-containing tetrahedra are:")
                          , indent 4 (pretty tets)
                                 ]))


    in
        fromMap tr res (L.map (map4 snd) theAssocs) 




makeEdgeNeighborhood
  :: Triangulation
     -> OIEdge
     -> GluingLabeller
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
            


polyprop_edgeIsGlued
  :: Eq b => SimplicialPartialQuotient b -> OIEdge -> Property
polyprop_edgeIsGlued m oiedge = 
    let
        oEdgeClass = eqv_equivalents (oEdgeEqv . spq_tr $ m) oiedge
        im_oiedge = spq_mapEd m oiedge
    in
        forAll (elements oEdgeClass)
            (\oiedge2 -> spq_mapEd m oiedge2 == im_oiedge)
        


        
--qc_EdgeCentered = $(quickCheckAll)

isRegardedAsSimplexByDisjointUnionDeriving ''DIM0 [t|EdgeNeighborhoodVertex|]

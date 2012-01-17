{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts, ViewPatterns, RecordWildCards #-}
{-# OPTIONS -Wall #-}
module SimplicialPartialQuotient where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.AscTuples
import Data.Map as M
import Data.Maybe
import HomogenousTuples
import INormalDisc
import NormalDisc
import PreRenderable
import PrettyUtil
import ShortShow
import Simplicial.SimplicialComplex
import Test.QuickCheck
import Triangulation
import TriangulationCxtObject
import Util
import qualified Data.List as L

-- | A simplicial map from the disjoint union of tetrahedra of a 'Triangulation' to some simplicial complex, identifying as most as many things as the gluings of the 'Triangulation'.
data SimplicialPartialQuotient v = SimplicialPartialQuotient {

    spq_tr :: Triangulation,
    -- | Need only be defined on the 'tIVertices' of the 'spq_tr'.
    spq_map :: IVertex -> v,
    spq_tets :: [Quadruple v]

}


spq_verts :: Ord v => SimplicialPartialQuotient v -> [v]
spq_verts = nub' . concatMap toList4 . spq_tets




fromMap
  :: Triangulation
     -> Map IVertex v -> [Quadruple v] -> SimplicialPartialQuotient v
fromMap t m tets = SimplicialPartialQuotient t (m !) tets


mapEdge
  :: (Vertices a, Verts a ~ Pair IVertex) =>
     SimplicialPartialQuotient b -> a -> Pair b
mapEdge (spq_map -> m) e = map2 m (vertices e)

mapTri
  :: (Vertices a, Verts a ~ Triple IVertex) =>
     SimplicialPartialQuotient b -> a -> Triple b
mapTri (spq_map -> m) tri = map3 m (vertices tri)


type GluingLabeller = NormalizedGluing -> String

makeTriangleLabelling
  :: (Show v, Pretty v, Ord v) =>
     SimplicialPartialQuotient v
     -> GluingLabeller -> Asc3 v -> Maybe TriangleLabel
makeTriangleLabelling spq gluingLabeller = triangleLabelsForSimplicial stlas
    where
        stlas = do
            gl@(tri,otri) <- extraGluings spq

            let lbl = gluingLabeller (normalizeGluing gl)

                (l ,r ,t ) = mapTri spq tri
                (l',r',t') = mapTri spq otri

            [sTriangleLabelAssoc lbl l  r  t  ,
             sTriangleLabelAssoc lbl l' r' t' ]


defaultGluingLabeller :: GluingLabeller
defaultGluingLabeller = 
    (++) <$> (show . getTIndex . ngDom) <*> (show . getTIndex . ngCod)


--             -- find the permutation that makes 'tri' resp. 'otri' ordered by brute force
--             g <- allS3
-- 
--             let theLabel = TriangleLabel lbl (inv g) 0.1 
--                 g_im_tri = g .* (mapTri spq tri)
--                 g_im_otri = g .* (mapTri spq otri)
-- 
--             (++)
--                 (guard (isOrdered3 g_im_tri)  >> [(g_im_tri ,theLabel)]) 
--                 (guard (isOrdered3 g_im_otri) >> [(g_im_otri,theLabel)]) 


-- | Gluings of the original triangulation which are *not* implemented by 'spq_map'
extraGluings
  :: Eq v => SimplicialPartialQuotient v -> [Gluing]
extraGluings spq = 
            L.filter 
                (not . isGluingImplemented spq)
                (tGluingsIrredundant (spq_tr spq))  

implementedGluings
  :: Eq v => SimplicialPartialQuotient v -> [Gluing]
implementedGluings spq = 
            L.filter 
                (isGluingImplemented spq)
                (tGluingsIrredundant (spq_tr spq))  

isGluingImplemented
  :: (Eq v) => SimplicialPartialQuotient v -> Gluing -> Bool
isGluingImplemented spq (tri,otri) = mapTri spq tri == mapTri spq otri 



notTooManyGluings
  :: Eq b => SimplicialPartialQuotient b -> Property
notTooManyGluings m =
    let
        oitris = tOITriangles (spq_tr m)
    in
        forAll (elements oitris)
        (\otri1 ->
            let
                im_otri1 = mapTri m otri1

                mglueds = L.filter (\otri2 -> otri2 /= otri1 && mapTri m otri2 == im_otri1) oitris
            in
                if L.null mglueds
                then property True
                else
                    forAll (elements mglueds)

                        (\otri2 ->
                            lookupGluingOfOITriangle (spq_tr m) otri1 == Just otri2))

-- prop_enoughGluings
--   :: (Ord b, Pretty b, Show b) => SimplicialPartialQuotient b -> Property
-- prop_enoughGluings spq =
--     let
--         _triangleLabel = makeTriangleLabelling spq defaultGluingLabeller
--     in
--         forAll (elements . tOriginalGluings . spq_tr $ spq)
--             (\(tri,otri) ->
-- 
--                 let
--                     im1 = mapTri spq tri 
--                     im2 = mapTri spq otri
--                     
--                     lbl1 = _triangleLabel im1
--                     lbl2 = _triangleLabel im2
--                 in
-- 
--                     label "equal images" (im1 == im2)
--                     .||.
--                     (isJust lbl1 && lbl1 == lbl2)
--                     
-- 
-- 
-- 
--             )


toSimplicialComplex
  :: (Pretty v, Ord v, Show v) => SimplicialPartialQuotient v -> SC3 v
toSimplicialComplex = fromTets . spq_tets   

toPreRenderable
  :: (Ord v, ShortShow v, Pretty v, Show v) =>
     SPQWithCoords v -> PreRenderable (SC3 v)
toPreRenderable (SPQWithCoords spq coords gluingLabeller) = 
            mkPreRenderableWithTriangleLabels 
                (makeTriangleLabelling spq gluingLabeller)
                coords
                (toSimplicialComplex spq)

    
spq_Equivalence_helper
  :: (Eq v, Ord a, GluingMappable a) =>
     (ITriangle -> [a])
     -> (Triangulation -> [a])
     -> SimplicialPartialQuotient v
     -> Equivalence a
spq_Equivalence_helper thingsOfTri allThings spq = 
    mkEquivalence 
        (do
            gl <- implementedGluings spq
            na <- thingsOfTri (fst gl)
            [(na,gluingMap gl na)])
        (allThings (spq_tr spq))



spq_INormalArcEquivalence
  :: Eq v => SimplicialPartialQuotient v -> Equivalence INormalArc
spq_INormalArcEquivalence = spq_Equivalence_helper normalArcList tINormalArcs

spq_INormalCornerEquivalence
  :: Eq v => SimplicialPartialQuotient v -> Equivalence INormalCorner
spq_INormalCornerEquivalence = spq_Equivalence_helper normalCornerList tINormalCorners




identitySPQ :: Triangulation -> SimplicialPartialQuotient IVertex
identitySPQ tr =

            SimplicialPartialQuotient
                tr
                id
                (fmap vertices (tTetrahedra_ tr))


-- | A 'SimplicialPartialQuotient' whose vertices have coordinates in @R^3@, and which knows how to label its triangles corresponding to gluings not implemented by the partial quotient
data SPQWithCoords v = SPQWithCoords {
    spqwc_spq :: SimplicialPartialQuotient v,
    spqwc_coords :: v -> Vec3,
    spqwc_gluingLabeller :: GluingLabeller
}


geometrifySingleTetTriang
  :: Triangulation -> GluingLabeller -> SPQWithCoords Vertex
geometrifySingleTetTriang tr gluingLabeller = 
    assert (tNumberOfTetrahedra tr == (1::Integer))
    $
        SPQWithCoords 
            (SimplicialPartialQuotient tr forgetTIndex [allVertices'])
            vertexDefaultCoords
            gluingLabeller

-- | Creates a partial quotient for the given 2-tetrahedron triangulation which implements the gluing of the given triangle (and no others) 
geometrifyTwoTetTriang
  :: Triangulation
     -> ITriangle -> GluingLabeller -> SPQWithCoords (T IVertex)
geometrifyTwoTetTriang tr theTri gluingLabeller = 
    assert (tNumberOfTetrahedra tr == (2::Integer))
    $
        SPQWithCoords 
            (SimplicialPartialQuotient 
                tr 
                p
                (fmap (map4 p . vertices . tindex) [0,1]))
            (M.fromList a !)
            gluingLabeller


    where
        cA,cB,cC,cD :: Vec3
        (cA,cB,cC,cD) = map4 vertexDefaultCoords (vA,vB,vC,vD)
        cD' = (2/3) *& (cA &+ cB &+ cC) &- cD

        theGluedTri = fromMaybe (error ("geometrifyTwoTetTriang:"++
                                        "second arg is a boundary triangle"))                    
                                        
                                (lookupGluingOfITriangle tr theTri)


        -- | Triangulation containing only the chosen gluing
        tr' = mkTriangulation 2 [(theTri,theGluedTri)]

        p = pMap tr' 


        (u0,u1,u2) = vertices theTri 
        u3 = iTriangleDualVertex theTri
        --(v0,v1,v2) = vertices theGluedTri
        v3 = oiTriangleDualVertex theGluedTri


        a = [ (p u0, cA)
            , (p u1, cB)
            , (p u2, cC)
            , (p u3, cD)
            , (p v3, cD')
            ]

instance Pretty v => Show (SimplicialPartialQuotient v) where
    showsPrec = prettyShowsPrec

instance Pretty v => Pretty (SimplicialPartialQuotient v) where
    pretty SimplicialPartialQuotient{..} = 
        prettyRecord "SimplicialPartialQuotient"
            [("spq_tr",pretty spq_tr),
             ("spq_map",prettyFunction spq_map (tIVertices spq_tr)),
             ("spq_tets",pretty spq_tets) 
            ]

instance (Ord v, Pretty v) => Show (SPQWithCoords v) where
    showsPrec = prettyShowsPrec

instance (Ord v, Pretty v) => Pretty (SPQWithCoords v) where
    pretty SPQWithCoords{..} =
        prettyRecord "SPQWithCoords"
            [("spqwc_spq",pretty spqwc_spq),
             ("spqwc_coords",prettyFunction spqwc_coords (spq_verts spqwc_spq)),
             ("spqwc_gluingLabeller",prettyFunction 
                                        (spqwc_gluingLabeller . normalizeGluing)
                                        (extraGluings spqwc_spq))
             
             ]


spqwc_tr :: SPQWithCoords v -> Triangulation
spqwc_tr = spq_tr . spqwc_spq

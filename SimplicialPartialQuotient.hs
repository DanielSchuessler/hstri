{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ViewPatterns #-}
module SimplicialPartialQuotient where

import Triangulation
import Test.QuickCheck
import qualified Data.List as L
import Data.Map as M
import PreRenderable
import Control.Monad
import HomogenousTuples
import Control.Arrow
import Simplicial.SimplicialComplex
import Data.Maybe
import Data.Vect.Double(Vec3)
import INormalDisc
import NormalDisc
import PrettyUtil

data SimplicialPartialQuotient v = SimplicialPartialQuotient {

    spq_tr :: Triangulation,
    spq_map :: IVertex -> v,
    spq_tets :: [Quadruple v]

}

spq_verts = nub' . concatMap toList4 . spq_tets

instance Show v => Show (SimplicialPartialQuotient v) where
    showsPrec = prettyShowsPrec

instance Show v => Pretty (SimplicialPartialQuotient v) where
    pretty spq = docAssocs 
                    (fmap 
                        (pretty &&& (text . show . spq_map spq)) 
                        (tIVertices (spq_tr spq))) 




fromMap
  :: Triangulation
     -> Map IVertex v -> [Quadruple v] -> SimplicialPartialQuotient v
fromMap t m tets = SimplicialPartialQuotient t (m !) tets


mapEdge
  :: Vertices a (Pair IVertex) =>
     SimplicialPartialQuotient b -> a -> Pair b
mapEdge (spq_map -> m) e = map2 m (vertices e)

mapTri
  :: Vertices a (Triple IVertex) =>
     SimplicialPartialQuotient b -> a -> Triple b
mapTri (spq_map -> m) tri = map3 m (vertices tri)



makeTriangleLabelling spq = flip M.lookup (M.fromList (triangleLabelList spq))


triangleLabelList :: Ord v => SimplicialPartialQuotient v -> 
    [(Triple v, TriangleLabel)]
triangleLabelList spq = do
--            (lbl,(tri,otri)) <- zip labels extraGluings
            (tri,otri) <- extraGluings spq

            let lbl = (show . getTIndex) tri ++ (show . getTIndex) otri

            -- find the permutation that makes 'tri' resp. 'otri' ordered by brute force
            g <- allS3

            let theLabel = TriangleLabel lbl (inv g) 0.1 
                g_im_tri = g .* (mapTri spq tri)
                g_im_otri = g .* (mapTri spq otri)

            (++)
                (guard (isOrdered3 g_im_tri)  >> [(g_im_tri ,theLabel)]) 
                (guard (isOrdered3 g_im_otri) >> [(g_im_otri,theLabel)]) 


-- | Gluings of the original triangulation which are *not* implemented by 'spq_map'
extraGluings spq = 
            L.filter 
                (not . isGluingImplemented spq)
                (tGluingsIrredundant (spq_tr spq))  

implementedGluings spq = 
            L.filter 
                (isGluingImplemented spq)
                (tGluingsIrredundant (spq_tr spq))  

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

prop_enoughGluings
  :: Ord v =>
     SimplicialPartialQuotient v -> Property
prop_enoughGluings spq =
    let
        _triangleLabel = makeTriangleLabelling spq 
    in
        forAll (elements . tGluings_ . spq_tr $ spq)
            (\(tri,otri) ->

                let
                    im1 = mapTri spq tri 
                    im2 = mapTri spq otri
                    
                    lbl1 = _triangleLabel im1
                    lbl2 = _triangleLabel im2
                in

                    label "equal images" (im1 == im2)
                    .||.
                    (isJust lbl1 && lbl1 == lbl2)
                    



            )


toSimplicialComplex = fromTets . spq_tets   

toPreRenderable spq coords = 
    let pr0 =
            mkPreRenderable 
                (coords . unOT)
                (toSimplicialComplex spq)
    in pr0
        {
            pr_triangleLabel = makeTriangleLabelling spq . unOT
        }


    
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



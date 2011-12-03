{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ViewPatterns, TypeOperators, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module EdgeCentered where
import Equivalence
import Triangulation
import Data.Map as M
import Data.List as L
import HomogenousTuples
import Test.QuickCheck
import Simplicial.Labels
import Simplicial.SimplicialComplex
import Data.Vect.Double hiding((.*))
import Data.Maybe
import Control.Monad
import Control.Arrow(first)

data EdgeNeighborhoodVertex =
    Bottom |
    Top |
    RingVertex Int

    deriving(Eq,Ord)

instance Show EdgeNeighborhoodVertex where
    show Bottom = "B"
    show Top = "T"
    show (RingVertex i) = show i


makeEdgeNeighborhoodMap :: Triangulation -> OIEdge -> Map IVertex EdgeNeighborhoodVertex
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




        res = loop 1 initialMap initialTet initialv0 initialv1 initialv3  
    in
        res



makeEdgeNeighborhood :: Triangulation -> OIEdge -> 
    (Map IVertex EdgeNeighborhoodVertex, 
     WithCoords (OTuple EdgeNeighborhoodVertex))

makeEdgeNeighborhood tr oiEdge =
    let

        m = makeEdgeNeighborhoodMap tr oiEdge

        n = tOIEdgeDegree tr oiEdge

        ds = fromTets ([ (Bottom,Top,RingVertex i,RingVertex (i+1))
                                    
                        | i <- [0..n-2] ]

                        ++ [(Bottom,Top,RingVertex 0,RingVertex (n-1))])


        h = 0.5

        coords0 (OT x) = coords x

        coords Bottom = (-h) *& vec3Z
        coords Top = h *& vec3Z
        coords (RingVertex i) = Vec3 (sin phi) (-(cos phi)) 0  
            where 
                phi = 2*pi*(2*i'-1)/(2*n')
                i' = fromIntegral i
                n' = fromIntegral n

        triangleLabel = flip M.lookup (M.fromList (fmap (first OT) triangleLabelList))

        triangleLabelList :: [(Triple EdgeNeighborhoodVertex, TriangleLabel)]
        triangleLabelList = do
--            (lbl,(tri,otri)) <- zip labels extraGluings
            (tri,otri) <- extraGluings

            let lbl = (show . getTIndex) tri ++ (show . getTIndex) otri

            -- find the permutation that makes 'tri' resp. 'otri' ordered by brute force
            g <- allS3

            let theLabel = TriangleLabel lbl (inv g) 0.1 
                g_im_tri = g .* (mapTri m tri)
                g_im_otri = g .* (mapTri m otri)

            (++)
                (guard (isOrdered3 g_im_tri)  >> [(g_im_tri ,theLabel)]) 
                (guard (isOrdered3 g_im_otri) >> [(g_im_otri,theLabel)]) 

        extraGluings = 
            L.filter (\(tri,otri) -> mapTri m tri /= mapTri m otri) (tGluingsIrredundant tr)  
        

--         labels = fmap return letters ++ join (liftM2 (\x y -> [x,y])) letters ++ error "out of labels :("
--         letters = "FGJPQR?" 

            
    in
        (m,addCoordFunc coords0 triangleLabel ds)


mapEdge :: (Ord a, Vertices a1 (Pair a)) => Map a b -> a1 -> Pair b
mapEdge m e = map2 (m !) (vertices e)
mapTri
  :: (Ord a, Vertices a1 (Triple a)) => Map a b -> a1 -> Triple b
mapTri m tri = map3 (m !) (vertices tri)



prop_notTooManyGluings
  :: Eq b => Triangulation -> Map IVertex b -> Property
prop_notTooManyGluings tr m =
    let
        oitris = tOITriangles tr
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
                            lookupGluingOfOITriangle tr otri1 == Just otri2))


prop_edgeIsGlued
  :: Eq b => Triangulation -> Map IVertex b -> OIEdge -> Property
prop_edgeIsGlued tr m oiedge = 
    let
        oEdgeClass = eqv_equivalents (oEdgeEqv tr) oiedge
        im_oiedge = mapEdge m oiedge
    in
        forAll (elements oEdgeClass)
            (\oiedge2 -> mapEdge m oiedge2 == im_oiedge)
        

prop_enoughGluings
  :: Eq v =>
     Triangulation -> (Map IVertex v, WithCoords (OTuple v)) -> Property
prop_enoughGluings tr (m,x) =
        forAll (elements (tGluings_ tr))
            (\(tri,otri) ->

                let
                    im1 = mapTri m tri 
                    im2 = mapTri m otri
                    
                    lbl1 = getTriangleLabel x (OT im1)
                    lbl2 = getTriangleLabel x (OT im2)
                in

                    label "equal images" (im1 == im2)
                    .||.
                    (isJust lbl1 && lbl1 == lbl2)
                    



            )

        
--qc_EdgeCentered = $(quickCheckAll)

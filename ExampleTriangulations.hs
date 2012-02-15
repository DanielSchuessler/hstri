{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
{-# OPTIONS -Wall #-}
module ExampleTriangulations where

import Blender.Types
import Control.Exception
import Data.Vect.Double
import EdgeCentered
import THUtil
import TriangulationCxtObject
import Data.SumType
import Util
import PreRenderable
import Numeric.AD.Vector
import Data.VectorSpace
import HomogenousTuples

tr_aroundEdge :: Word -> Triangulation
tr_aroundEdge n =
 mkTriangulation n
    [ (tindex i ./ tABD,
       tindex (mod (i+1) n) ./ oABC) 
        
        | i <- [0..n-1]
    ]            

tr_octahedron :: Triangulation
tr_octahedron = tr_aroundEdge 4 

tr_aroundEdge_topVertex, tr_aroundEdge_bottomVertex :: Word -> TVertex
tr_aroundEdge_bottomVertex n = pMap (tr_aroundEdge n) (0 ./ vB)
tr_aroundEdge_topVertex n = pMap (tr_aroundEdge n) (0 ./ vA)

tr_aroundEdge_centralEdge_preimage :: OIEdge
tr_aroundEdge_centralEdge_preimage = 0 ./ oedge (vB,vA)

tr_aroundEdge_centralEdge :: Word -> T OIEdge
tr_aroundEdge_centralEdge n = pMap (tr_aroundEdge n) tr_aroundEdge_centralEdge_preimage

tr_octahedronWithGluing :: Triangulation
tr_octahedronWithGluing = fromRight $ addGluings tr_octahedron 
    [ oiTriangleGluing (0 ./ oCDA) (0 ./ oCDB) ]

spqwc_aroundEdge :: Word -> SPQWithCoords EdgeNeighborhoodVertex
spqwc_aroundEdge n = makeEdgeNeighborhood (tr_aroundEdge n) tr_aroundEdge_centralEdge_preimage show 

octahedronCam1 :: Cam
octahedronCam1 =
    Cam
        (Vec3 0.217 (-3.091) 2.071)
        (Vec3 1.0077831745147705 0 0.06999944895505905)
        defaultFOV

tr_oneTet :: Triangulation
tr_oneTet = mkTriangulation 1 []

tr_twoTets :: Triangulation
tr_twoTets = mkTriangulation 2 [gluing (0./tABC) (1./oABC)]

spqwc_oneTet :: SPQWithCoords Vertex
spqwc_oneTet = oneTetWithDefaultCoords tr_oneTet show

spqwc_twoTets :: SPQWithCoords TVertex
spqwc_twoTets = spqwc_twoTetBipyramid tr_twoTets (0./tABC) show

spqwc_l31 :: SPQWithCoords TVertex
spqwc_l31 = spqwc_twoTetBipyramid tr_l31 (0./tABC)
                (\(ngDom -> (viewI -> I i tri)) ->
                    assert (i==0) $
                        case tri of
                             _ | tri == tABD -> "F"
                               | tri == tACD -> "G"
                               | tri == tBCD -> "J"
                               | otherwise -> assert False undefined)
    
twoTetCam' :: Cam
twoTetCam' = 
 Cam (uncurry3 Vec3 (-0.23205919563770294, -3.6175613403320312, 0.15333208441734314))
     (uncurry3 Vec3 (1.4570116996765137, -8.902474064598209e-07, -0.0009973797714337707))
     defaultFOV

twoTetCam :: Cam
twoTetCam =
 readCam "(Vector((0.42211657762527466, -1.9336755275726318, 0.5852480530738831)), Euler((1.1714677810668945, 1.1168522178195417e-06, 0.3451415002346039), 'XYZ'), 0.8575560591178853)"


oneTetCam :: Cam
oneTetCam = 
 Cam (uncurry3 Vec3 (-0.23205919563770294+0.2, -3.6175613403320312, 0.15333208441734314+0.28))
     (uncurry3 Vec3 (1.4570116996765137, -8.902474064598209e-07, -0.0009973797714337707))
     defaultFOV

l41 :: SPQWithCoords Vertex
l41 = oneTetWithDefaultCoords 
        (mkTriangulation 1 [(0./tBCD,0./oCDA),(0./tABD,0./oBCA)])
        (\gl@(ngDom -> forgetTIndex -> t) -> 
            case () of
                    _ | t==tBCD -> "F"
                      | t==tABD -> "G"
                      | otherwise -> error ("l41 "++ $(showExps 'gl)) 
                      )



torus3 :: SPQWithCoords EdgeNeighborhoodVertex
torus3 = makeEdgeNeighborhood tr oiedge show
    where
        oiedge = 0 ./ oedge (vA,vB) :: OIEdge

        -- modified (oriented?) version of tr_184'
        tr = mkTriangulation 6 [(0 ./ tBCD, 1 ./ oCBD), (0 ./ tACD, 2 ./ oCDB), (0 ./ tABD, 3 ./ oDCB), (0 ./ tABC, 4 ./ oCDB), (1 ./ tBCD, 0 ./ oCBD), (1 ./ tACD, 2 ./ oDCA), (1 ./ tABD, 3 ./ oCDA), (1 ./ tABC, 5 ./ oCDB), (2 ./ tBCD, 0 ./ oDAC), (2 ./ tACD, 1 ./ oDCA), (2 ./ tABD, 4 ./ oCDA), (2 ./ tABC, 5 ./ oDCA), (3 ./ tBCD, 0 ./ oDBA), (3 ./ tACD, 1 ./ oDAB), (3 ./ tABD, 5 ./ oBCA), (3 ./ tABC, 4 ./ oCBA), (4 ./ tBCD, 0 ./ oCAB), (4 ./ tACD, 2 ./ oDAB), (4 ./ tABD, 5 ./ oADB), (4 ./ tABC, 3 ./ oCBA), (5 ./ tBCD, 1 ./ oCAB), (5 ./ tACD, 2 ./ oCBA), (5 ./ tABD, 4 ./ oADB), (5 ./ tABC, 3 ./ oDAB)]

-- | Equals tr_184 in ClosedOrCensus6
tr_184' :: LabelledTriangulation
tr_184' = ("T x S1 : #1", mkTriangulation 6 [(0 ./ tBCD, 1 ./ oCBD), (0 ./ tACD, 2 ./ oCDB), (0 ./ tABD, 3 ./ oCDB), (0 ./ tABC, 4 ./ oDCB), (1 ./ tBCD, 0 ./ oCBD), (1 ./ tACD, 2 ./ oDCA), (1 ./ tABD, 3 ./ oDCA), (1 ./ tABC, 5 ./ oDCB), (2 ./ tBCD, 0 ./ oDAC), (2 ./ tACD, 1 ./ oDCA), (2 ./ tABD, 4 ./ oDCA), (2 ./ tABC, 5 ./ oCDA), (3 ./ tBCD, 0 ./ oDAB), (3 ./ tACD, 1 ./ oDBA), (3 ./ tABD, 4 ./ oDBA), (3 ./ tABC, 5 ./ oBDA), (4 ./ tBCD, 0 ./ oCBA), (4 ./ tACD, 2 ./ oDBA), (4 ./ tABD, 3 ./ oDBA), (4 ./ tABC, 5 ./ oACB), (5 ./ tBCD, 1 ./ oCBA), (5 ./ tACD, 2 ./ oCAB), (5 ./ tABD, 3 ./ oCAB), (5 ./ tABC, 4 ./ oACB)])


lensSpace :: Word -> Word -> SPQWithCoords EdgeNeighborhoodVertex
lensSpace p (tindex -> q) = 
        makeEdgeNeighborhood tr (0 ./ oedge(vB,vA)) 
        (\gl -> '#':if (ngDomTet gl, ngCodTet gl) == (0,tindex p-1)
                   then show (p-1)
                   else (show . ngDomTet) gl) 

    where
        tr = mkTriangulation p (verticals ++ _outer) 

        add i j = mod (i+j) (tindex p)

        verticals = [ gluing (i ./ tABD) (add i 1 ./ oABC) | i <- [0..tindex p-1] ] 

        _outer = [ gluing (i ./ tACD) (add i q ./ oBCD) | i <- [0..tindex p-1] ]
                 



                

defaultTetImmersion :: GeneralTetImmersion
defaultTetImmersion = GTetE 10
    ((\(Tup4 xs) -> 
        sumV (zipWith (\x v -> x *^ liftVec3 (vertexDefaultCoords v)) (toList4 xs) allVertices)
        )

        . unitToStd3)

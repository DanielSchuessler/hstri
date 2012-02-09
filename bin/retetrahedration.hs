{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ImplicitParams #-}
import HsTri 
import TypeLevel.TF.Nat.Small
import Simplicial.AnySimplex
import Data.Vect.Double.Util.Dim3
import Data.Vect.Double.Base
import Data.Maybe
import Control.Exception



spqwc = l41
tr = spqwc_tr spqwc

ns = 0./Q_ac

main = do
    go preCut
    let ?retetrahedrate = False in go blenderabel
    let ?retetrahedrate = True in go blenderabel



go =            testBlender 
            .   setCams [ oneTetCam ] 
            .   defaultScene 
            .   transformCoords (rotate3 (-0.0*pi) vec3X . rotate3 (-pi*0.45) vec3Z)





blenderabel = pseudomanifoldStyle $
    fragmentAC 
    `disjointUnion`
    fragmentBD

--    `disjointUnion` toPreRenderable spqwc

type PreCutSimplex = Either1 (OTuple Vertex) (OTuple (Corn Vertex))

preCut :: Blenderable PreCutSimplex 
preCut =
    fromSpqwc spqwc `disjointUnion`
    fromIntegerNormalSurface spqwc ns 


    
inj1 :: Vertex -> PreCutSimplex N0
inj1 = Left1 . OT 
inj2 :: Corn Vertex -> PreCutSimplex N0
inj2 = Right1 . OT 

(a,b,c,d) = map4 inj1 allVertices'

cor :: Edge -> PreCutSimplex N0
cor = inj2 . uncurry (corn 0 1) . (vertices :: Edge -> Pair Vertex)




stla_f1 = (sTriangleLabelAssoc "F1" b (cor eBC) d) {stla_scale=1.2}
stla_f2 = (sTriangleLabelAssoc "F2" (cor eBC) c (cor eCD)) {stla_scale=1.2}
stla_f3 = (sTriangleLabelAssoc "F3" (cor eBC) (cor eCD) d) {stla_scale=1.2}

stla_g1 = (sTriangleLabelAssoc "G1" a (cor eAB) (cor eAD)) {stla_scale=1.2}
stla_g2 = (sTriangleLabelAssoc "G2" (cor eAB) b d) {stla_scale=1.2}
stla_g3 = (sTriangleLabelAssoc "G3" (cor eAD) (cor eAB) d) {stla_scale=1.2}

(gluingMapBCD,gluingMapABD) = 
    let
        f t = gluingMap (t, fromJust (lookupGluingOfITriangle tr t))
    in
        map2 f (0./tBCD,0./tABD)


fragmentAC, fragmentBD :: (?retetrahedrate :: Bool) => PreRenderable (OTuple (PreCutSimplex N0))

fragmentAC = 
    transformCoords (&- cutNormalDirDisplacement) $
    mkFragment (tetrahedrate3Prism (a,cor eAB, cor eAD) (c,cor eBC, cor eCD))
        [
         stla_f2, gluingMapBCD stla_f1, gluingMapBCD stla_f3 
        ,stla_g1, gluingMapABD stla_g2, gluingMapABD stla_g3
        ]

fragmentBD =
    transformCoords (&+ cutNormalDirDisplacement) $
    mkFragment (flip tetrahedrate3Prism (b,cor eAB, cor eBC) (d,cor eAD, cor eCD))

    [ stla_f1, stla_f3, gluingMapBCD stla_f2 
    , stla_g2, stla_g3, gluingMapABD stla_g1 
    ]


cutNormalDirDisplacement = 
    let
        (u,v,w,_) = map4 (coords . cor . normalCornerGetContainingEdge)
                            (normalCorners . forgetTIndex $ ns)
    in
        0.25 * normalize (crossprod (v-u) (w-u))
        

coords = ba_coords preCut

mkFragment (tets, newEdges, newTris) stlas =
    let

        ds :: SimplicialComplex (PreCutSimplex N0)
        ds = fromTets tets


        visible
            | ?retetrahedrate = const True
            | otherwise = 
                foldAnySimplex3
                    (const True)
                    (not . (`elem` map sort2 newEdges) . unOT)
                    (not . (`elem` map sort3 newTris) . unOT)
                    (assert False undefined)
               

    in
        (mkPreRenderable (coords . unOT) ds)
            { pr_visible = visible
            , pr_triangleLabel = triangleLabelsForSimplicial stlas }
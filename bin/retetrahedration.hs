{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS -Wall -fno-warn-unused-imports -fno-warn-missing-signatures #-}
import HsTri 
import TypeLevel.TF.Nat.Small
import Data.Vect.Double.Util.Dim3
import Data.Vect.Double.Base
import Data.Maybe
import Control.Exception
import Data.Lens.Common
import CheckAdmissibility
import Data.Cross
import Numeric.AD.Classes(Mode)
import Text.Groom
import THBuild
import Control.Monad
import MathUtil
import Util



tr = snd tr_0

spqwc = oneTetWithDefaultCoords tr f
    where
        f g | ngDomOrCodIs (0 ./ tABC) g = "F" -- glued to ABD
            | ngDomOrCodIs (0 ./ tACD) g = "G" -- glued to BCD
            | otherwise = $undef


theQuad = Q_ab
ns = toAdmissible stdCoordSys tr (0./theQuad)

main = do
    go 1024 "cut0before.png" preCut
    let ?retetrahedrate = False in go 1024 "cut0after.png" blenderabel
    let ?retetrahedrate = True in go 1800 "cut0rt.png" blenderabel



go res fn =         void 
            .   blenderMain DoRender 
            .   setRenderFilepath ("/h/dipl/pictures/"++fn)
            .   setRenderRes res res
            .   setCams [ cam ] 
            .   setLamps [ lamp ]
            .   defaultScene 



    where
        cam = readCam "(Vector((-0.35092324018478394, 2.026278018951416, -0.6763539910316467)), Euler((1.8966331481933594, -5.88849570704042e-06, 3.3061158657073975), 'XYZ'), 0.8575560591178853)"

        lamp = defaultLamp {
                    lamp_eulers = eulerAnglesXYZ (-0.25*pi) 0 (-pi/6)
                }





blenderabel = mkBlenderable pseudomanifoldStyle $
    fragmentAB 
    `disjointUnion`
    fragmentCD

--    `disjointUnion` toPreRenderable spqwc


type PreCutComplex = DJSC2
                        Vertex (Corn Vertex)
                        (Asc2 Vertex) (Asc2 (Corn Vertex))
                        (Asc3 Vertex) (Asc3 (Corn Vertex))
             
type PreCutVert = DJSimp DIM0 Vertex (Corn Vertex)

preCut :: Blenderable PreCutComplex
preCut =
    fromSpqwc spqwc `disjointUnion`
    fromIntegerNormalSurface spqwc ns 


    
inj1 :: Vertex -> PreCutVert
inj1 = DJSimp . Left
inj2 :: Corn Vertex -> PreCutVert
inj2 = DJSimp . Right


cor :: Edge -> PreCutVert
cor = inj2 . uncurry (corn 0 1) . (vertices :: Edge -> Pair Vertex)



(gluingMapABC,gluingMapACD) = 
    let
        f t = gluingMap ($fromJst (lookupGluingAsGluing tr t))
    in
        map2 f (0./tABC,0./tACD)


_scale = 0.8

a,b,c,d :: PreCutVert
(a,b,c,d) = map4 inj1 allVertices'

stla_f1 = (sTriangleLabelAssoc "F1" a (cor eBC) (cor eAC)) {stla_scale=_scale}
stla_f2 = (sTriangleLabelAssoc "F2" a b (cor eBC)) {stla_scale=_scale}
stla_f3 = (sTriangleLabelAssoc "F3" (cor eAC) (cor eBC) c) {stla_scale=_scale}

stla_g1 = (sTriangleLabelAssoc "G1" a (cor eAC) (cor eAD)) {stla_scale=_scale}
stla_g2 = (sTriangleLabelAssoc "G2" (cor eAC) c d) {stla_scale=_scale}
stla_g3 = (sTriangleLabelAssoc "G3" (cor eAD) (cor eAC) d) {stla_scale=_scale}

stlas :: [SimplicialTriangleLabelAssoc PreCutVert]
stlas = let     
            fs = $(slistE [ "stla_f"++show i  | i <- [1::Int ..3]])
            gs = $(slistE [ "stla_g"++show i  | i <- [1::Int ..3]])
        in 
            map gluingMapABC fs ++ fs ++
            map gluingMapACD gs ++ gs


fragmentAB,fragmentCD
  :: (?retetrahedrate::Bool) =>
     PreRenderable (SC3 (DJSimp DIM0 Vertex (Corn Vertex)))
fragmentAB = 
    transformCoords (^+^ cutNormalDirDisplacement) $
    mkFragment (tetrahedrate3Prism (a,cor eAC, cor eAD) (b,cor eBC, cor eBD))

fragmentCD =
    transformCoords (^-^ cutNormalDirDisplacement) $
    mkFragment (flip tetrahedrate3Prism (c,cor eAC, cor eBC) (d,cor eAD, cor eBD))



cutNormalDirDisplacement
  :: (Floating (s Double), Mode s) => Tup3 (s Double)
cutNormalDirDisplacement = 
    let
        (u,v,w,_) = map4 (liftVec3 . coords . cor . normalCornerGetContainingEdge)
                            (normalCorners theQuad)
    in
        0.25 *^ twoNormalize (cross3 (v^-^u) (w^-^u))
        

coords :: Vert PreCutComplex -> Vec3
coords = ba_coordsSimple preCut

mkFragment :: (?retetrahedrate :: Bool) =>
     ([Quadruple PreCutVert],
      [Pair PreCutVert],
      [Triple PreCutVert])
     -> PreRenderable (SC3 (DJSimp DIM0 Vertex (Corn Vertex)))
mkFragment (tets, newEdges, newTris) =
    let

--         ds :: SimplicialComplex (Vert PreCutComplex)
        ds = fromTets tets


        visible 
            | ?retetrahedrate = const Visible
            | otherwise = 
                foldAnySimplex2
                    (const Visible)
                    (visibleIf . not . (`elem` map asc2 newEdges))
                    (visibleIf . not . (`elem` map asc3 newTris))
--                     (assert False undefined)
               
        triangleLabels = triangleLabelsForSimplicial stlas

    in
--         trace ("stlas =\n"++groom stlas) $
--         trace ("triangleLabels =\n"++prettyString (prettyFunction triangleLabels (triangles ds))) $


            pr_setVisibility visible
        .   (if ?retetrahedrate then setL pr_triangleLabelL triangleLabels else id) 
        $   mkPreRenderable (coords) ds




ab = let ?retetrahedrate = False in fragmentAB




{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell #-}
import Data.Lens.Common
import Control.Category((>>>))
import TupleTH
import Data.Vect.Double.Util.Dim3
import Data.Vect.Double.Base
import FileLocation
import qualified Data.Map as M
import ExampleTriangulations
import SimplicialPartialQuotient
import EdgeCentered
import HomogenousTuples
import Blender
import TriangulationCxtObject
import Data.Vect.Double.Interpolate
import Data.Vect.Double.Util.Dim2
import Debug.Trace
import Control.Exception
import THUtil

main = edge20

pachner32 = 
    let
        before = 
               -- rotateAngleAxis (pi/3) vec3Z $ 
                spqwc_aroundEdge 3

        after :: SPQWithCoords EdgeNeighborhoodVertex
        after = 
            
            setL (spqwc_spqL >>> spq_tetsL) 
                 [ $(catTuples 1 3) Bottom  (map3 EquatorVertex (0,1,2)) 
                 , $(catTuples 1 3) Top     (map3 EquatorVertex (0,1,2)) 
                 ]
                 before
        
            --mkPreRenderableWithTriangleLabels (const Nothing)
                
            
    in do

        testBlender (setCam cam . defaultScene $ before) 
        testBlender (setCam cam . defaultScene $ after) 

cam :: Cam
cam = readCam "(Vector((1.6595690250396729, -5.473565101623535, 0.8606469035148621)), Euler((1.4290757179260254, 3.992030087829335e-06, 0.2902868092060089), 'XYZ'), 0.8575560591178853)"

 
        

collapseEdge =
    let
        before = spqwc_aroundEdge 4

        step0 = mkBlenderable pseudomanifoldStyle 
                . transformCoords f0 
                . pr_makeEmbeddingsGeneral (const 25)
                . toPreRenderable $ before

        f0 (Vec3 x y z) = Vec3 x y (z * (norm (Vec2 x y)))

    in do
        testBlender (setCam cam . defaultScene $ step0)


tr_vertex20 = mkTriangulation 2 
                [ gluing' (0./t) (1./t) | t <- take 3 allTriangles ]

vertex20 =
    let
        tr = tr_vertex20

        p = pMap tr

        coords :: TVertex -> Vec3
        coords =
                    flip $indxShow
                        (M.fromList
                            [
                                (p (0./vA), zero)
                            ,   (p (0./vB), vec3X)
                            ,   (p (0./vC), rotate3 (2*pi/3) vec3Z vec3X)
                            ,   (p (0./vD), rotate3 (4*pi/3) vec3Z vec3X)
                            ]

                            )

        gtes :: TTriangle -> Maybe GeneralTriangleEmbedding
        gtes = flip M.lookup
                (M.fromList $ 
                    [
                        (p (0./tBCD), GTE res (dome vec3Z))
                      , (p (1./tBCD), GTE res (dome (negate vec3Z)))

                    ])

        res = 100


        dome dir = 
            withBaryCoords (\tuv@(Vec3 t u v) -> 
                let
                    f x = x^2 / normsqr tuv
                in
                    f t *& coords (p (0./vB))
                    &+
                    f u *& coords (p (0./vC))
                    &+
                    f v *& coords (p (0./vD))
                    &+
                    (10*t*u*v) *& dir)



        before :: PreRenderable Triangulation
        before = 
                pr_setTriangleEmbedding gtes $
        
                mkPreRenderable coords tr


        after = 
            let
                t = p (0./tBCD)
            in
                pr_hide 
                    ( foldAnySimplex2 
                                (not . (`isSubface` t)) 
                                (not . (`isSubface` t)) 
                                (/= t))

                .

                   pr_setTriangleEmbedding (const Nothing)
--                 pr_setTriangleEmbedding (flip lookup [ (t, GTE 2 . dome $ zero) ]) 


                    $ before
                


    in do
        testBlender (setCam cam20 . defaultScene . mkBlenderable pseudomanifoldStyle $ before) 
        testBlender (setCam cam20 . defaultScene . mkBlenderable pseudomanifoldStyle $ after) 


cam20 = readCam "(Vector((0.370949923992157, -2.3874809741973877, 0.7168295383453369)), Euler((1.273403286933899, 6.27131976216333e-06, 0.13297684490680695), 'XYZ'), 0.8575560591178853)"


edge20 = 
    let
        tr = tr_aroundEdge 2
        p = pMap tr

        coords :: TVertex -> Vec3
        coords =
                    flip $indxShow $
                        M.fromList
                            [
                                (p (0./vA), vec3Z)
                            ,   (p (0./vB), -vec3Z)
                            ,   (p (0./vC), -vec3X) -- = p (1./vD)
                            ,   (p (0./vD), vec3X)  -- = p (1./vC)
                            ]

                            

        gtes :: TTriangle -> Maybe GeneralTriangleEmbedding
        gtes = flip M.lookup
                (M.fromList $ 
                    [
                        (p (0./tACD), GTE res (sphericalTriangle (-1) 1))
                      , (p (0./tBCD), GTE res (sphericalTriangle (-1) (-1)))
                      , (p (1./tACD), GTE res (sphericalTriangle 1 1))
                      , (p (1./tBCD), GTE res (sphericalTriangle 1 (-1)))

                      , (p (0./tABC), GTE res (semidisk (-1)))
                      , (p (0./tABD), GTE res (semidisk 1))

                    ])

        res = 20


        sphericalTriangle ydir zdir (Vec2 u v) = 
            let
                -- latitude. runs from 0 (zdir*vec3Y) to 1 (equator, z = 0)
                s = u+v     

                -- longitude. runs from -1 to 1 for each fixed 's', spanning a 
                -- half circle in a constant z plane, from negative x to positive x 
                d = if s == 0
                       then 0
                       else (u-v)/s 

                s' = s*pi/2
                d' = d*pi/2
            in
                Vec3 (sin s' * sin d') (ythickness * ydir * sin s' * cos d') (zdir * cos s') 

        ythickness = 0.5 -- 1 = sphere 

        semidisk xdir (Vec2 bness cness {- cness is representative for cness or dness #-}) = 
            let
                aness = 1-bness-cness 


                -- runs from 0 to 1
                abness = aness+bness 

                abnessDistorted = abness


                -- runs from -1 to 1
                a_vs_b = if abness == 0
                            then 0
                            else (aness-bness)/abness 


                xz_0 = -- if we're close to c (low abness), draw circular arcs
                                (let 
                                    phimax = acos (abnessDistorted/2) 
                                    Vec2 foo bar = sinCosRadius (a_vs_b*phimax) abnessDistorted
                                 in  Vec2 (1-foo) bar)

                xz_1 = -- if we're close to ab, draw lines
                                (let x = 1-abnessDistorted 
                                     zbounds = sqrt' (1 - x^2)
                                     -- x^2 + zbounds^2 = 1
                                     z = zbounds*a_vs_b 
                                 in
                                    Vec2 x z)

                xz@(Vec2 x z) = interpolate abness xz_0 xz_1
                                
            in
                $(assrt [| norm xz < 1+1E-10 |] ['xz,'xz_0,'xz_1]) $
                Vec3 (xdir * x) 0 z



        before :: PreRenderable Triangulation
        before = 
                pr_setTriangleEmbedding gtes $
        
                mkPreRenderable coords tr


        after = 
            let
                t0 = p (0./tABC)
                t1 = p (0./tABD)
            in
                pr_hide 
                    ( foldAnySimplex2 
                                (\x -> not (x `isSubface` t0 || x `isSubface` t1))
                                (\x -> not (x `isSubface` t0 || x `isSubface` t1))
                                (\x -> not (x `elem` [t0,t1])))

                

--                 . pr_setTriangleEmbedding (const Nothing)
--                 . pr_setTriangleEmbedding (flip lookup [ (t, GTE 2 . dome $ zero) ]) 


                    $ before
                
        cam_edge20 = readCam "(Vector((4.056652069091797, -1.6806788444519043, 1.085201621055603)), Euler((1.3310924768447876, 1.799694018700393e-06, 1.1548134088516235), 'XYZ'), 0.8575560591178853)"


    in do
--         testBlender (setCam cam_edge20 . defaultScene . mkBlenderable pseudomanifoldStyle $ before) 
        testBlender (setCam cam_edge20 . defaultScene . mkBlenderable pseudomanifoldStyle $ after) 


-- floating-point-retardant sqrt
sqrt' x = case compare x 0 of
               LT -> trace ("sqrt' "++show x) 0 
               EQ -> 0
               GT -> sqrt x

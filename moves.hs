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

main = vertex20

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


edge20 = let
    tr = undefined

    in
        undefined

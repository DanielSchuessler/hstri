{-# LANGUAGE TemplateHaskell #-}
import HsTri
import Data.Lens.Common
import Control.Category((>>>))
import TupleTH
import Data.Vect.Double.Util.Dim3
import Data.Vect.Double.Base


pachner32 = 
    let
        before = 
               -- rotateAngleAxis (pi/3) vec3Z $ 
                spqwc_aroundEdge 3
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

cam = readCam "(Vector((1.6595690250396729, -5.473565101623535, 0.8606469035148621)), Euler((1.4290757179260254, 3.992030087829335e-06, 0.2902868092060089), 'XYZ'), 0.8575560591178853)"

 
        
main = collapseEdge

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

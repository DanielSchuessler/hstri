{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, GADTs, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving, ExtendedDefaultRules #-} 


import HsTri
import Data.Vect.Double.Util.Dim3
import Data.Function
import Data.Vect.Double.Interpolate

--ns1 = 1 :* (0 ./ ntA) :+  1./Q_ad
ns1 = 0 ./ Q_ac :+ 1./ntB
ns2 = 0 ./ Q_ad :+ 1./ntA

spqwc = spqwc_twoTets

ba =
        fromIntegerNormalSurface spqwc ns1 
        `disjointUnion`
        mkBlenderable normalSurfaceStyleB ( 
            let
                pr = normalSurfaceToPreRenderable spqwc ns2

                interpVertex r = 
                    interpolate r `on` 
                    spqwc_coords' spqwc
            in
                pr {
                    pr_coords0 = \c ->   
                        case map2 (forgetTIndex . unT) (cornVerts c) of
                             (A,C) -> 
                                interpVertex 0.8 (0./A) (0./C)

                             (A,B) -> 
                                interpVertex 0.2 (0./A) (0./B)

                             (C,D) -> 
                                interpVertex 0.2 (0./C) (0./D)
                            
                             _ -> pr_coords0 pr c
                })

main = 
    testBlender . 
       setCams [Cam (-2.8 *& vec3Y) ((pi/2) *& vec3X) defaultFOV] . 
       defaultScene .
       transformCoords rot $
       (fromSpqwc spqwc 
        `disjointUnion`
        ba
        )
            
       

       
       
        --`disjointUnion` abnormalCurve
  where
    rot = rotate3 (pi*0.05) vec3X . rotate3 (pi*0.42) vec3Z



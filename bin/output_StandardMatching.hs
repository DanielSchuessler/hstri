import HsTri
import Data.Vect.Double.Util.Dim3
import Data.Vect.Double.Interpolate
import Numeric.AD.Types
import Numeric.AD.Vector
import CheckAdmissibility
import CoordSys

spqwc = spqwc_twoTets

tr = spqwc_tr spqwc


main = testBlender 
            . setCams [cam] 
            . setLamps [oldDefaultLamp] 
            . defaultScene 
            . transformCoords t
            $
        fromSpqwcAndIntegerNormalSurface spqwc (toAdmissible stdCoordSys tr ns)

            
ns =    2 :* 0./ntA :+      0./Q_ad
    :+       1./ntA :+ 2 :* 1./Q_ad


(cA,cB,cC,cD1) = map4 (spqwc_coords' spqwc) (0./vA, 0./vB, 0./vC, 1./vD)

t :: FF Tup3 Tup3 Double
t = liftVec3 

        . rotate3 pi (vec3X &- vec3Y)
        . rotate3 (-pi/3) cD1
        . tup3toVec3
        . fmap realToFrac



--  . rotate3 (0.3*pi) (cA &- interpolate 0.5 cB cC)

cam = readCam "(Vector((-0.170100137591362, -1.1018648147583008, 0.392932265996933)), Euler((1.470848798751831, 8.333342520927545e-06, -0.3187176287174225), 'XYZ'), 1.221730947271014)"

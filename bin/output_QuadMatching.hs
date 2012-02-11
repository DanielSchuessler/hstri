{-# LANGUAGE TupleSections, ExtendedDefaultRules #-}
import HsTri

n = 4

vTop = (tr_aroundEdge_topVertex n)
vBot = (tr_aroundEdge_bottomVertex n)


tr = tr_aroundEdge n
spqwc = spqwc_aroundEdge n


linkBot = vertexLinkingSurface vBot
linkTop = vertexLinkingSurface vTop

is = [ 0.. 3]

upQuads = standardCoordinates [i./Q_ac | i <- is]
downQuads = standardCoordinates [i./Q_ad | i <- is]
irrQuads = standardCoordinates [i./Q_ab | i <- is]

irrelevants = 
    sumV
    ( irrQuads
     :
     [ vertexLinkingSurface (pMap tr (i ./ vC)) | i <- is]) 

orderProblem = canonExt tr (quad_fromNormalSurface  [0./Q_ac, 1./Q_ad, 2./Q_ad, 3./Q_ac]) 


-- orderProblemCam =
--  (Vector((-2.74153470993042, -3.1508467197418213, 0.28193315863609314)), Euler((1.5094698667526245, -9.01907424122328e-06, -0.7160851359367371), 'XYZ'))

-- orderProblemCam2 =
    -- (Vector((-4.257680416107178, -0.0048864055424928665, 0.33231738209724426)), Euler((1.4978764057159424, -1.8658225826584385e-06, -1.6015570163726807), 'XYZ'))

main = (testBlender . setCams [octahedronCam1] . setLamps [oldDefaultLamp])
        (defaultScene
            (
                fromSpqwcAndIntegerNormalSurface spqwc
                    orderProblem
            )
        )
    

{-# LANGUAGE TupleSections, ExtendedDefaultRules #-}
import HsTri
import CheckAdmissibility
import Util
import Data.Lens.Common

n = 4

vTop = (tr_aroundEdge_topVertex n)
vBot = (tr_aroundEdge_bottomVertex n)


tr      = tr_aroundEdge n
spqwc   = spqwc_aroundEdge n


linkBot = vertexLinkingSurface vBot
linkTop = vertexLinkingSurface vTop

is = [ 0.. 3]

upQuads     = standard_toAdmissible tr [i./Q_ac | i <- is]
downQuads   = standard_toAdmissible tr [i./Q_ad | i <- is]
irrQuads    = standard_toAdmissible tr [i./Q_ab | i <- is]

-- irrelevants = 
--     sumV
--     ( irrQuads
--      :
--      [ vertexLinkingSurface (pMap tr (i ./ vC)) 
--         | i <- is ] 
--     ) 

orderProblem = canonExt (toAdmissible undefined tr [0./Q_ac, 1./Q_ad, 2./Q_ad, 3./Q_ac]) 


orderProblemCam =
 (readCam "(Vector((-1.6148051023483276, -1.8704835176467896, 0.17721043527126312)), Euler((1.5094704627990723, -1.1930494110856671e-05, -0.7161185145378113), 'XYZ'), 0.8575560591178853)")

orderProblemCam2 = 
 (readCam "(Vector((-2.44828200340271, 0.06654377281665802, 0.20027251541614532)), Euler((1.5070645809173584, -2.145008693332784e-06, -1.6015543937683105), 'XYZ'), 0.8575560591178853)")

    `cam_withNormOf` orderProblemCam

go fn cam =    
        blenderMain JustLook 
    .   setRenderFilepath fn
    .   setRenderRes 1400 1400
    .   setCams [cam] 
    .   setLamps [mkSunObj (eulerAnglesXYZ (5*pi/12) 0 (-75*pi/180))  ]
    .   defaultScene

go' fn cam s = go fn cam ( 
    
        makeTrisInvisible        
        (fromSpqwc spqwc) 
    `disjointUnion`

     modTrisTransp (fmap (setL fresnelL 1.5 . setL alphaL 0.9 . setL spec_alphaL 0.9)) 
        (fromIntegerNormalSurface noCornPosOverride spqwc s) 

    )


main = do
    go' "/h/dipl/pictures/quadMatchingOrderSol.png" orderProblemCam orderProblem
    go' "/h/dipl/pictures/quadMatchingOrderSol2.png" orderProblemCam2 orderProblem
    

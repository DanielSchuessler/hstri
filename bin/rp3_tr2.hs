{-# LANGUAGE NoMonomorphismRestriction #-}
import HsTri
import ShortShow

tr = snd tr_6

p = pMap tr

ns = standard_toAdmissible tr (2 :* (0./ntC :+ 0./ntD :+ 1./Q_ad))

cns = toConcrete ns

(spqwc,tr') = spqwc_twoTetBipyramid' tr (0./tABC) shortShow

ba = fromSpqwcAndIntegerNormalSurface noCornPosOverride spqwc ns

main = do
    print tr'
    testBlender
        . defaultScene
        $ ba 


na1 = toOrderedFace $ 1 ./ normalArcByTriangleAndVertex tABD vB
-- (Corn 0 1 { A_0, D_1 } { B_1 },Corn 0 1 { B_0, A_1 } { B_1 }); using second.

na2 = toOrderedFace $ 1 ./ normalArcByTriangleAndVertex tABC vA 
-- (Corn 0 1 { B_0, A_1 } { C_0, C_1 },Corn 0 1 { B_0, A_1 } { B_1 }); using second.

cna1 = concrete ns 0 na1
cna2 = concrete ns 0 na2


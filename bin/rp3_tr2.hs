{-# LANGUAGE NoMonomorphismRestriction #-}
import HsTri
import ShortShow

tr = snd tr_6

p = pMap tr

ns = standard_toAdmissible tr (0./ntC :+ 0./ntD :+ 1./Q_ad) 

spqwc = spqwc_twoTetBipyramid tr (0./tABC) shortShow

ba = fromSpqwcAndIntegerNormalSurface noCornPosOverride spqwc ns

main =
    testBlender
    . defaultScene
    $ ba 

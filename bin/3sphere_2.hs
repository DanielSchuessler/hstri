import HsTri
import ShortShow
import Triangulation.FacetGluing

tr = snd tr_1

spqwc = oneTetWithDefaultCoords tr f
    where
        f g | ngDomOrCodIs (0 ./ tABC) g = "F"
            | ngDomOrCodIs (0 ./ tACD) g = "G"


main = do
    testBlender . setCam oneTetCam . defaultScene . fromSpqwc $ spqwc

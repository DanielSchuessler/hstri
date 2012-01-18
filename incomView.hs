import HsTri
import qualified Data.Vector as V
import DotUtil
import TriArcGraph
import EdgeCentered
import Data.Char

incom :: StandardCoordinates Integer
incom = 
      stc_fromDenseList tr
    . map (read . return :: Char -> Integer)
    . filter isDigit
    $ "0010; 000||0000; 100||0001; 000||1000; 000||0000; 100||0100; 000"


viewIncom =
                testBlender 
                (defaultScene
                    (fromSpqwcAndIntegerNormalSurface spqwc incom))

main = do
        let ddRes = dd tr
        pr . ppDDRes $ ddRes 
        V.forM_ (ddSolutions' ddRes)
            --(\sol -> viewDot (ta tr (Just sol)))
            (\w -> 
               let
                    ns = canonExtDbg tr (quad_fromVector tr w)
                in
                testBlender 
                (defaultScene
                    (fromSpqwcAndIntegerNormalSurface spqwc ns)))

        putStrLn "ok"
            



(_,tr) = tr_184'

spqwc = makeEdgeNeighborhood tr oiedge glab
    where
        oiedge = 0 ./ oedge (vA,vB) :: OIEdge

        glab ng =
                  (show . getTIndex . ngDom) ng
                  ++
                  (show . getTIndex . ngCod) ng
                 

tr_e = mkTriangulation 2 [(0 ./ tABD, 1 ./ oCDB), (1 ./ tABD, 0 ./ oACB), (0 ./ tBCD, 1 ./ oDCA)]

quad_e = quad_singleton (0./Q_ab) 1


import HsTri
import qualified Data.Vector as V
import Control.Monad

tr =
 mkTriangulation 3 
    [ (2 ./ tABD, 0 ./ oACD)
    , (1 ./ tACD, 2 ./ oCAD)
    , (1 ./ tABC, 2 ./ oCBA)
    , (1 ./ tBCD, 2 ./ oCBD)
    , (1 ./ tABD, 0 ./ oDAB)]   

qd = qd_fromList [1,0,0,0,0,0,0,0,0] :: QuadDenseI

main = print (canonExtDbg tr qd)

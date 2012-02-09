import HsTri
import qualified Data.Vector as V
import Control.Monad.Cont
import Data.Vect.Double.Base
import System.IO
import EdgeCentered

spqwc10 = transformCoords ((diag (Vec3 1 1 3.5) :: Mat3) `lmul`) $ lensSpace 10 1
spqwc = lensSpace 5 1

main = viewFundEdgeSolutionsInBlender spqwc
            (const . Just . edToAnySimplex2 . left' . asc2 $ (Bottom,Top)) 

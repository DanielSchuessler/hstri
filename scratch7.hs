import Math.GaussElim
import Test.QuickCheck
import Data.Maybe
import Data.Ratio
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

--main = print . maybeToList $ solve V.empty (V.empty :: V.Vector Rational)

main = print (fromJust $ solve 
                (V.fromList [V.fromList [1,1], V.fromList [0,1]])  
                (V.fromList [0,1] :: V.Vector Rational))

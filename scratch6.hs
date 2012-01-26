import HsTri
import qualified Data.Vector as V
import Control.Monad
import Tests
import Test.QuickCheck
import System.Random


main = quickCheckWith stdArgs { replay = Just (mkStdGen 0, 30) } 


            prop_eulerCharViaConcrete

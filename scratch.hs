import MathUtil
import Test.QuickCheck
import System.Random

main = quickCheckWith stdArgs { 
            maxSuccess = 50, 
            replay = Just (mkStdGen 10,135) 
        } prop_toEchelon

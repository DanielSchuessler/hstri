import Util
import Control.Monad
import GHC.Exts

main = findJust (\x -> guard (even x) >> return x) 
                [1::Int,2,3]
                `seq`
                return ()







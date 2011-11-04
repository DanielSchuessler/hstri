
-- import TriangulationCxtObject
-- import Test.QuickCheck
-- import System.Random
-- import Control.Monad
import qualified FaceLattice
import Control.Monad
import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.DeepSeq
import System.Random
import Triangulation
-- 
-- seed = 238987121
-- 
-- qcArgs sz = stdArgs { replay = Just (mkStdGen seed,sz), maxSuccess = 20 }
-- 
-- main = replicateM 10 (quickCheckWith (qcArgs 50) prop_IsSubface_VE_welldefined)


main = print (rnf (go `fmap` [1..200]))
    where
        go seed = 
                let t = unGen arbitrary (mkStdGen seed) 260 :: Triangulation
                    fl = FaceLattice.faceLattice t
                in fl

    

{-# LANGUAGE CPP #-}
import Data.List
import Control.Monad
import EitherC
import Data.Functor.Identity
import Data.Maybe
import Control.Exception
import System.IO.Unsafe

#define MK(name,l,r) name x 0 = l (show x); name x y = r (x + y)

MK(fError,error,)
MK(fIdentity,error,Identity)
MK(fMaybe,(const Nothing),return)
MK(fEither,Left,return)
MK(fEitherC,failureWithoutLoc,return)
MK(fIO,(throwIO . ErrorCall),return)

n = 100000 :: Int


main = 
    replicateM 10000 $
    print ((\n -> runEitherC' error id $ 
        foldl' (\r x -> liftM2 (+) r (fEitherC 1 x)) (return 0) [1..n]) n) 

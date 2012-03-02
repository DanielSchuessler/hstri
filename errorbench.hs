{-# LANGUAGE CPP #-}
import Criterion.Main
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

#define B(namestr,name,run) (bench namestr (nf (\n -> run $ foldl' (\r x -> liftM2 (+) r (name 1 x)) (return 0) [1..n]) n)) 

main = 
    defaultMain
        [ 
--           bench "error" 
--             (nf (\n -> foldl' (\r x -> r + fError 1 x) 0 [1..n]) n)
--         , B ("error/Identity",fIdentity,runIdentity) 
--         , B ("Maybe",fMaybe,fromJust) 
--         , B ("Either",fEither,either error id) 
--         , B ("IO",fIO,unsafePerformIO)
--         , 
          B ("EitherC",fEitherC,(\x -> runEitherC x error id)) 
        ]

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FaceIx where
import Test.QuickCheck
import System.Random
import Control.Applicative


newtype FaceIx = FI Int
    deriving(Num,Enum,Show,Eq,Ord,Arbitrary,Random)

runFI :: FaceIx -> Int
runFI (FI i) = i

genFaceIx n = FI <$> choose (0,n)

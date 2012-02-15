{-# LANGUAGE StandaloneDeriving, TypeFamilies, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module PrettyUtil.Matrix(
    module PrettyUtil,

     prettyStringMatrix,
    prettyStringMatrixWithColumnWidth,
    prettyMatrix,
    prettyMatrixWithColumnWidth,
    prettyVector,
    prettyVectorWithColumnWidth,
    prMatrix,
     blankingZeros,
                showRational,


    PrettyMatrix(..),
    PrettyVector(..),
    PrettyScalar(..),
    MaxColumnWidth(..),


    ) where


import Data.List(intercalate)
import Data.Ratio
import Element
import GHC.Show
import Numeric
import Test.QuickCheck(Arbitrary)
import Text.PrettyPrint.ANSI.Leijen hiding((<$>),Pretty(..),(<>))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed(Unbox)
import qualified Data.DList as DL
import Data.DList(DList)
import qualified Data.Vector.Generic as VG
import PrettyUtil

class MaxColumnWidth a where
    -- | Returns 0 if there are no columns
    maxColumnWidth :: a -> Int 
    maxColumnWidthList :: [a] -> Int 
    maxColumnWidthList [] = 0
    maxColumnWidthList xs = maximum (map maxColumnWidth xs)


prettyStringMatrix
  :: (AsList (Element a), AsList a, Element (Element a) ~ String) =>
     a -> String
prettyStringMatrix (asListOfLists -> xss) = 
    prettyStringMatrixWithColumnWidth 
        (maxColumnWidth xss)
        xss

instance MaxColumnWidth Char where 
    maxColumnWidth = const 1
    maxColumnWidthList = length

instance MaxColumnWidth a => MaxColumnWidth [a] where 
    maxColumnWidth = maxColumnWidthList

instance MaxColumnWidth a => MaxColumnWidth (V.Vector a) where 
    maxColumnWidth v | VG.null v = 0
                     | otherwise = VG.maximum . VG.map maxColumnWidth $ v

instance (Unbox a, MaxColumnWidth a) => MaxColumnWidth (VU.Vector a) where 
    maxColumnWidth v | VG.null v = 0
                     | otherwise = VG.maximum . VG.map maxColumnWidth $ v

instance (MaxColumnWidth a) => MaxColumnWidth (DList a) where 
    maxColumnWidth = maxColumnWidth . DL.toList
    

prettyStringMatrixWithColumnWidth
  :: (AsList (Element a), AsList a, Element (Element a) ~ [Char]) =>
     Int -> a -> [Char]
prettyStringMatrixWithColumnWidth columnWidth (asListOfLists -> xss) =
            (if (length xss<=1) then drop 1 else id) -- no leading newline if only one row
        .   concatMap ('\n':)
        .   map (intercalate " " . map fmtCell) 
        $   xss 
    where
        fmtCell = pad columnWidth




-- data ZeroPrinting = ShowZeros | BlankZeros

class MaxColumnWidth a => PrettyScalar a where
    prettyScalar :: a -> String


blankingZeros :: (Eq a, Num a) => (a -> [Char]) -> a -> [Char]
blankingZeros _ 0 = ""
blankingZeros f x = f x

showRational :: Integral a => Ratio a -> String
showRational x = 
        let 
            n = numerator x
            d = denominator x 
        in
            if d==1         then show n
            else if n==1    then '/':show d
            else if n==(-1) then '-':'/':show d
            else                 show n ++ '/' : show d

-- showRational :: (Show a, Integral a) => Ratio a -> [Char]
-- showRational 0 = "0"
-- showRational x = if x < 0 then "-"++showPositiveRational (-x) else showPositiveRational x
-- 
-- 
-- showPositiveRational :: (Show a, Integral a) => Ratio a -> String
-- showPositiveRational x =
--                 case (numerator x,denominator x) of
--                      (1,2) -> "½"
--                      (1,3) -> "¿"
--                      (2,3) -> "¿"
--                      (1,4) -> "¼"
--                      (3,4) -> "¾"
--                      (1,5) -> "¿"
--                      (2,5) -> "¿"
--                      (3,5) -> "¿"
--                      (4,5) -> "¿"
--                      (1,6) -> "¿"
--                      (5,6) -> "¿"
--                      (1,8) -> "¿"
--                      (3,8) -> "¿"
--                      (5,8) -> "¿"
--                      (n,1) -> show n
--                      (1,d) -> "¿"++show d
--                      (n,d) -> show n ++ "/" ++ show d
                                
instance MaxColumnWidth Rational where
    maxColumnWidth = length . blankingZeros prettyScalar

instance MaxColumnWidth Integer where
    maxColumnWidth = length . blankingZeros prettyScalar

instance MaxColumnWidth Int where
    maxColumnWidth = length . blankingZeros prettyScalar

instance MaxColumnWidth Double where
    maxColumnWidth = length . blankingZeros prettyScalar

instance PrettyScalar Rational where
    prettyScalar = showRational

instance PrettyScalar Integer where
    prettyScalar = show

instance PrettyScalar Int where
    prettyScalar = show

instance PrettyScalar Double where
    prettyScalar 0 = "0"
    prettyScalar x = showFFloat (Just 4) x "" 

prettyMatrix = 
    prettyStringMatrix . prettyScalarss

prettyScalarss = (map . map . blankingZeros) prettyScalar . asListOfLists

prettyMatrixWithColumnWidth columnWidth = 
    prettyStringMatrixWithColumnWidth columnWidth . prettyScalarss 

prettyVectorWithColumnWidth
  :: (Eq (Element xs), Num (Element xs), AsList xs, PrettyScalar (Element xs)) =>
     Int -> xs -> [Char]
prettyVectorWithColumnWidth columnWidth = 
    prettyMatrixWithColumnWidth columnWidth . (:[])

newtype PrettyMatrix a = PrettyMatrix { unPrettyMatrix :: a }
    deriving Arbitrary

instance
     (
      AsList (Element xss),
      AsList xss,
      PrettyScalar (Element (Element xss)),
      Eq (Element (Element xss)),
      Num (Element (Element xss))
      )

     =>
     
     Show (PrettyMatrix xss) where

     show = prettyMatrix . unPrettyMatrix

prettyVector = prettyMatrix . map (:[]) . asList 

newtype PrettyVector a = PrettyVector { unPrettyVector :: a }
    deriving Arbitrary

instance
 (AsList xs, PrettyScalar (Element xs),
      Eq (Element xs),
      Num (Element xs))
    => Show (PrettyVector xs) where

     show = prettyVector . unPrettyVector

prMatrix = putStrLn . prettyMatrix 


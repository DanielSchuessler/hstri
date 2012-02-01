{-# LANGUAGE BangPatterns, TemplateHaskell, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
module Math.GaussElim.Tests where

import Math.GaussElim
import Test.QuickCheck
import Control.Exception
import Control.DeepSeq
import PrettyUtil
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import Data.Vector(Vector) 
import qualified Data.List as L
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed(Unbox)
import qualified Data.DList as DL
import Data.Monoid
import qualified Data.Foldable as Fold
import Data.Foldable(Foldable)
import THUtil
import Math.SparseVector
import qualified Data.Set as S
import Data.Set(Set)
import Control.Monad.ST.Safe
import Control.Monad
import Data.Maybe
import Data.VectorSpace
import Test.QuickCheck.All
import Control.Applicative
import QuickCheckUtil
import Data.Char


isEchelon :: (Num a, VG.Vector v a) => Vector (v a) -> Bool
isEchelon mtx = adjacentsSatisfy rel (V.map leftmostNonzero mtx) 

    where
        leftmostNonzero = VG.findIndex (/= 0)

        rel Nothing x = isNothing x
        rel (Just _) Nothing = True
        rel (Just x) (Just y) = x < y


idRatMat :: f (f' Rational) -> f (f' Rational)
idRatMat = id

prop_isEchelon :: Property
prop_isEchelon = expectFailure (forAll arbMatrix' (isEchelon . idRatMat . unPrettyMatrix))



arbMatrix r c = (PrettyMatrix . V.fromList . map V.fromList) 
    <$> vectorOf r (vectorOf c smallInt)

smallInt = fromIntegral <$> choose (-4,4::Int)

arbSquareMatrix = join arbMatrix

intSqrt :: Int -> Int
intSqrt = round . (id :: Double -> Double) . sqrt . fromIntegral


arbMatrix' = sized (\n -> join (join (liftM2 arbMatrix) (choose (1,max 1 (intSqrt n)))))

instance Arbitrary r => Arbitrary (Vector r) where
    arbitrary = V.fromList <$> ((:) <$> arbitrary <*> arbitrary)
    shrink = VG.mapM shrink 


instance (Num r, Arbitrary r) => Arbitrary (BMatrix r) where
    arbitrary = unPrettyMatrix <$> arbMatrix' 
    shrink = VG.mapM shrink 

prop_toEchelon :: Property
prop_toEchelon = 
    forAll arbMatrix' 
        (\(PrettyMatrix mtx) -> case toReducedEchelon mtx of
                      (mtx',erts,_) ->
                          printTestCase ("mtx' =\n"++prettyMatrix mtx') $
                          printTestCase ("erts =\n"++show erts) $
                            mtx' .=. applyERTs erts mtx
                            .&&.
                            isEchelon mtx')

prop_symSol :: PrettyMatrix (BMatrix Rational) -> Property
prop_symSol (PrettyMatrix mtx) =
    case symbolicSolution mtx (variable . return . chr . (+ ord 'a')) of
            ((mtx',_,_),s) ->
                printTestCase ("mtx' =\n"++prettyMatrix mtx') $
                printTestCase (unlines ("s =":
                                    map show (VG.toList s)++[";"])) $

                    mulMVwith (*^) mtx s .=. V.replicate (rows mtx) zeroV

prop_solve :: PrettyMatrix (BMatrix Rational) -> Property
prop_solve (PrettyMatrix mtx) = 

    forAllShrink 
        ((PrettyVector . V.fromList) <$> vectorOf (rows mtx) smallInt) 
        shrink
        
        (\(PrettyVector rhs) ->

                printTestCase 
                    (case toEchelon mtx of
                        (emtx,erts,pcis) -> 
                            unlines [
                                "echelon form ="++(show $ prettyMatrix' emtx),
                                "pivot column ixs ="++show pcis,
                                "rhs' ="++(prettyVector . applyERTsV erts $ rhs)
                                ]) $

                case solve mtx rhs of
                     Nothing -> property True
                     Just !x ->
                         printTestCase ("x = "++show (prettyVector' x)) $
                         mulMVwith (*) mtx x .=. rhs)



qc_GaussElim :: IO Bool
qc_GaussElim = $quickCheckAll

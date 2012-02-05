{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Triangulation.Random
    (
     
     randT,randomClosed,                                                
     randomManifoldT,
     arbitraryTriangulation,arbitraryClosedTriangulation,shrinkTriangulation,
     genTet, arbitraryManifoldTriangulation,

     ClosedTriangulation(..),
     ManifoldTriangulation(..),

     randomTriangulation,
    )


    where

import ClosedOrCensus6
import ClosedNorCensus8
import Collections(elemOfSetAt,deleteAt)
import Control.Applicative
import Control.Monad.State
import Data.Set(Set)
import System.Random
import THUtil
import Test.QuickCheck
import Test.QuickCheck.Gen
import Triangulation
import Util
import qualified Data.Set as S
import Data.Maybe
import QuickCheckUtil
import Triangulation.Class
import Triangulation.VertexLink
import Data.SumType

arbitraryTriangulation :: Gen Triangulation
arbitraryTriangulation = arbitraryTriangulationG (\nTets -> choose (0,2*nTets)) 

arbitraryClosedTriangulation :: Gen Triangulation
arbitraryClosedTriangulation = arbitraryTriangulationG (\nTets -> return (2*nTets))

newtype ClosedTriangulation = ClosedTriangulation Triangulation
    deriving (ToTriangulation,Show)

instance Arbitrary ClosedTriangulation where 
    arbitrary = ClosedTriangulation <$> arbitraryClosedTriangulation

arbitraryTriangulationG :: (Int -> Gen Int) -> Gen Triangulation
arbitraryTriangulationG nGluingsGen =
    sized (\n ->
        let
            small = (<= max 1 (n `div` 5)) . tNumberOfTetrahedra
        in
            frequency $ 
                (2,
                    do
                        nTets <- choose (1::Int,max 1 (n`div`5))
                        nGluings <- nGluingsGen nTets
                        randT nTets nGluings)

                : mapMaybe 
                    (\(w,census) ->
                        case filter small . map snd $ census of
                             [] -> Nothing
                             ts -> Just (w, elements ts))

                    [(2, closedOrCensus6)
                    ,(1, closedNorCensus8)]
                
                )



shrinkTriangulation :: Triangulation -> [Triangulation]
shrinkTriangulation t = 
--            concatMap removeTet (tTetrahedra_ t) ++ 
            mkTriangulation (tNumberOfTetrahedra t) <$> shrink (tGluingsIrredundant t)


randT :: Int -> Int -> Gen Triangulation
randT nTets nGluings = 
        $(assrt [| nTets > 0 |] 'nTets) $
        $(assrt [| nGluings >= 0 |] 'nGluings) $
        $(assrt [| nGluings <= 2*nTets |] ['nGluings,'nTets]) $
            
            generateUntilJust (sumTypeToMaybe <$> go)
    where
        go = do
            let 
                tets = (tindex . fromIntegral) <$> [0..nTets-1]

                loop :: [Gluing] -> Int -> StateT (Set ITriangle) Gen [Gluing] 
                loop acc 0 = return acc
                loop acc j = do
                    t1 <- takeTriangle
                    t2 <- takeTriangle
                    g <- lift arbitrary
                    loop ((t1,packOrderedFace t2 g):acc) (j-1)

                takeTriangle = do
                    trianglesLeft <- get
                    ix <- lift (choose (0, S.size trianglesLeft-1))
                    let res = elemOfSetAt ix trianglesLeft
                    put (deleteAt ix trianglesLeft)
                    return res


            pairs <- evalStateT (loop [] nGluings) (S.fromList ((./) <$> tets <*> allTriangles))
            
            return $ mkTriangulationSafe (fi nTets) pairs

randomClosed :: Int -> IO Triangulation
randomClosed n = randomTriangulation n (2*n)

randomTriangulation :: Int -> Int -> IO Triangulation
randomTriangulation nTets nGluings = do
    g <- newStdGen 
    return $ unGen (randT nTets nGluings) g 0 --(error "randomTriangulation: undefined") 

instance Arbitrary Triangulation where
    arbitrary = arbitraryTriangulation 
    shrink = shrinkTriangulation


genI ::  Arbitrary a => Triangulation -> Gen (I a)
genI t = liftM2 I (genTet t) arbitrary

genTet ::  Triangulation -> Gen TIndex
genTet = elements . tTetrahedra_ 

newtype ManifoldTriangulation t = ManifoldTriangulation t
    deriving (Show,ToTriangulation)

-- | Uses ' arbitraryManifoldTriangulation '
instance (Arbitrary t, ToTriangulation t) => Arbitrary (ManifoldTriangulation t) where
    arbitrary = ManifoldTriangulation <$> arbitraryManifoldTriangulation

-- | Generates only manifold triangulations.
arbitraryManifoldTriangulation :: (Arbitrary t, ToTriangulation t) => Gen t
arbitraryManifoldTriangulation = arbitrary `suchThat` isManifoldTriangulation 

randomManifoldT :: Int -> Int -> Int -> Gen Triangulation
randomManifoldT n minGl maxGl =
     (randT n =<< choose (minGl,maxGl)) `suchThat` isManifoldTriangulation

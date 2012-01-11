{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Triangulation.Random
    (
     randomTriangulation,randT,randomClosed,                                                
     arbitraryTriangulation,shrinkTriangulation,
     genTet

    )


    where

import Triangulation
import Test.QuickCheck
import ClosedOrCensus6
import Control.Applicative
import qualified Data.List as L
import THUtil
import Control.Monad.State
import qualified Data.Set as S
import Data.Set(Set)
import Collections(elemOfSetAt,deleteAt)
import System.Random
import Test.QuickCheck.Gen
    

arbitraryTriangulation :: Gen Triangulation
arbitraryTriangulation =
        frequency 
            [(1,
                sized (\n -> do
                    nTets <- choose (1::Int,max 1 (n`div`5))
                    nGluings <- choose (0,2*nTets)
                    randT nTets nGluings
                    ))
            ,(1,
                snd <$> elements closedOrCensus6)
             
            ]



shrinkTriangulation :: Triangulation -> [Triangulation]
shrinkTriangulation t = 
--            concatMap removeTet (tTetrahedra_ t) ++ 
            fmap removeGluing (tOriginalGluings t)
        where
            removeGluing g = 
                mkTriangulation (tNumberOfTetrahedra t) (L.delete g (tOriginalGluings t))

generateUntilRight :: Show a => Gen (Either a b) -> Gen b
generateUntilRight g = fromRight <$> (g `suchThat` isRight)

randT :: Int -> Int -> Gen Triangulation
randT nTets nGluings = 
        $(assrt [| nTets > 0 |] 'nTets) $
        $(assrt [| nGluings >= 0 |] 'nGluings) $
        $(assrt [| nGluings <= 2*nTets |] ['nGluings,'nTets]) $
            
            generateUntilRight go
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


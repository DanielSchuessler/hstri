{-# LANGUAGE FlexibleContexts, ViewPatterns, ExistentialQuantification, StandaloneDeriving, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module Tests.Gens where

--import QuadCoordinates.Class
import Triangulation.AbstractNeighborhood
import ClosedNorCensus8
import ClosedOrCensus6
import ConcreteNormal
import Control.Monad.State
import Data.Function
import Data.List as L
import Data.Maybe
import Data.Ord
import Math.SparseVector 
import QuadCoordinates.CanonExt
import QuadCoordinates.Dense
import QuadCoordinates.MatchingEquations
import QuickCheckUtil
import StandardCoordinates
import StandardCoordinates.Dense
import StandardCoordinates.MatchingEquations
import Test.QuickCheck
import Test.QuickCheck.All
import Triangulation.Random
import Triangulation.VertexLink
import TriangulationCxtObject
import VerboseDD
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Triangulation.Class
import StandardCoordinates.SurfaceQueries
import Util
import Control.Applicative
import Data.Typeable
import Data.SumType

data TriangulationWithUnrestrictedStandardCoords r
    = 
    
    forall s. (Show s, UpdatableStandardCoords s s r) => 
    TriangulationWithUnrestrictedStandardCoords
        Triangulation
        s

deriving instance Show (TriangulationWithUnrestrictedStandardCoords r) 

arbitraryI
  :: (Arbitrary a, HasTIndex b a, ToTriangulation t) => t -> Gen b
arbitraryI (toTriangulation -> tr) = 
    (./) <$> (tindex <$> choose (0,tNumberOfTetrahedra_ tr - 1)) <*> arbitrary

arbitraryINormalDisc :: ToTriangulation tr => tr -> Gen INormalDisc
arbitraryINormalDisc = arbitraryI

arbitraryStandardDense :: Arbitrary r => Triangulation -> Gen (StandardDense V.Vector r)
arbitraryStandardDense tr = sd_fromList <$> vector (tNumberOfNormalDiscTypes tr)


arbitraryStandardSparse :: (Eq r, Num r, Arbitrary r) => Triangulation -> Gen (SparseVector INormalDisc r)
arbitraryStandardSparse tr =
                    do
                        k <- choose (0,tNumberOfTetrahedra tr)
                        sparse_fromAssocs 
                            <$> vectorOf k
                                    ((,) <$> arbitraryINormalDisc tr <*> arbitrary) 

instance (Num r, Show r, Ord r, Arbitrary r, Typeable r) => Arbitrary (TriangulationWithUnrestrictedStandardCoords r) 
    where

        arbitrary = do
            tr <- arbitrary

            oneof [
                TriangulationWithUnrestrictedStandardCoords tr <$> arbitraryStandardDense tr,
                TriangulationWithUnrestrictedStandardCoords tr <$>                  arbitraryStandardSparse tr ] 

forAllCensusTriangulations
  :: Testable prop => (LabelledTriangulation -> prop) -> Property
forAllCensusTriangulations = forAllClosedCensusTriangulations

forAllClosedCensusTriangulations
  :: Testable prop => (LabelledTriangulation -> prop) -> Property
forAllClosedCensusTriangulations p = 
    forAllElementsShrink closedOrCensus6 p .&.
    forAllElementsShrink closedNorCensus8 p



eitherFuncToProp
  :: (Show a, Show (L ab), SubSumTy ab) => (a -> ab) -> a -> Property
eitherFuncToProp f x =
    printTestCase (show x) $
    either' (\e -> printTestCase (show e) False) (const (property True)) (f x)

data FundamentalEdgeSolution tr = 
        FundamentalEdgeSolution tr (Admissible StandardDenseI)
    deriving Show

instance (ToTriangulation tr, Arbitrary tr) => Arbitrary (FundamentalEdgeSolution tr) where
    arbitrary = do
        (tr,sols) <- (do
                        tr <- arbitraryManifoldTriangulation
                        return (tr, fundEdgeSolutions tr))

                        `suchThat` 
                                (not . VG.null . snd)

        sol <- elementsV sols                            
        return (FundamentalEdgeSolution tr sol)


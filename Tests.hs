{-# LANGUAGE ViewPatterns, ExistentialQuantification, StandaloneDeriving, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}
module Tests where

--import QuadCoordinates.Class
import AbstractNeighborhood
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
import Tests.Gens


prop_qVertexSolutions (ManifoldTriangulation tr) =
    tNumberOfTetrahedra_ tr <= 10 ==>

    let
        sols = qVertexSolutions tr
    in
        conjoin
            (map (eitherFuncToProp (quad_admissible tr)) (V.toList sols))

prop_qVertexSolutionExts (ManifoldTriangulation tr) =
    tNumberOfTetrahedra_ tr <= 10 ==>
                                                               
    let
        sols = qVertexSolutionExts tr
    in 
        conjoin
            (map (eitherFuncToProp (admissible tr)) (V.toList sols))


qc_Tests :: IO Bool
qc_Tests = $quickCheckAll

prop_censuses_manifoldTriangulations :: Property
prop_censuses_manifoldTriangulations =
    forAllCensusTriangulations isManifoldTriangulation

prop_censuses_closedManifoldTriangulations :: Property
prop_censuses_closedManifoldTriangulations =
    forAllClosedCensusTriangulations isClosedManifoldTriangulation

prop_innerEdgeNeighborhood :: Triangulation -> Property
prop_innerEdgeNeighborhood (tr :: Triangulation) =
    forAllElements (edges tr)
        (\te -> case innerEdgeNeighborhood te of
                     Nothing -> property (isBoundaryEdge te)
                     Just xs -> length xs .=. ecSize te)

  where
    isBoundaryEdge = 
        any (isNothing . lookupGluingOfITriangle tr)
            . itrianglesContainingEdge


-- | Test that each element of krBasis satisfies the matching equations
prop_krBasisMatchingEquations ::  Triangulation -> Gen Prop
prop_krBasisMatchingEquations t =
        (let
            krB = krBasis t :: [StandardCoordinates Integer]
            mE = matchingEquations t
         in
            conjoin' [ (x <.> y) == 0 
                        | x <- krB
                        , y <- mE ])






prop_eulerCharViaConcrete 
    (FundamentalEdgeSolution (ManifoldTriangulation (tr :: Triangulation)) surf) =

        let
            cverts = concreteCorners surf
            carcs = concreteArcs surf
            ctris = concreteTris surf
            cquads = concreteQuads surf

            distinctsAfterPMap :: (Ord a, TriangulationDSnakeItem a) => [a] -> Int
            distinctsAfterPMap = S.size . S.fromList . map (pMap tr)
        in
            eulerC surf .=. 

            toInteger (
                    distinctsAfterPMap cverts 
                +   distinctsAfterPMap ctris
                +   distinctsAfterPMap cquads
                -   distinctsAfterPMap carcs 
               )



prop_iNormalDiscInGluinglessTriangulationIsDisk :: Property
prop_iNormalDiscInGluinglessTriangulationIsDisk = do
    n <- choose (1,10)
    let tr = mkTriangulation n []
    i <- choose (0,fi n - 1)
    d <- elements allNormalDiscs
    property (isDisk (toAdmissible tr (i ./ d)))



        


prop_partialCanonicalPart_idempotent (TriangulationWithUnrestrictedStandardCoords tr s) =
    forAllElements (vertices tr)
        (\v -> partialCanonicalPart v s .=. partialCanonicalPart v (partialCanonicalPart v s))

prop_partialCanonicalPart_commute (TriangulationWithUnrestrictedStandardCoords tr s) =
    join forAllElements2 (vertices tr)
        (\(v,w) -> 
            partialCanonicalPart v (partialCanonicalPart w s)
                .=.
            partialCanonicalPart w (partialCanonicalPart v s))

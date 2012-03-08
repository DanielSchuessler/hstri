{-# LANGUAGE FlexibleContexts, ViewPatterns, ExistentialQuantification, StandaloneDeriving, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}
module Tests where

--import QuadCoordinates.Class
import CheckAdmissibility
import ClosedNorCensus8
import ClosedOrCensus6
import ConcreteNormal
import Control.Applicative
import Control.Monad.State
import Data.BitVector.Adaptive
import Data.Function
import Data.List as L
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.Typeable
import Math.SparseVector 
import PrettyUtil
import QuadCoordinates.CanonExt
import QuadCoordinates.Class
import QuadCoordinates.Dense
import QuadCoordinates.MatchingEquations
import QuadCoordinates.SolSetConversion
import QuickCheckUtil
import StandardCoordinates
import StandardCoordinates.Dense
import StandardCoordinates.MatchingEquations
import StandardCoordinates.SurfaceQueries
import Test.QuickCheck
import Test.QuickCheck.All
import Tests.Gens
import Triangulation.AbstractNeighborhood
import Triangulation.Class
import Triangulation.Random
import Triangulation.VertexLink
import TriangulationCxtObject
import Util
import VectorUtil
import VerboseDD
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.List.NonEmpty(NonEmpty(..))
import Control.Arrow(second)

prop_quadToStandardSolSet :: Property
prop_quadToStandardSolSet =
    forAllCensusTriangulations (\(_,tr) ->
        let
            qs = dd tr
            expected = sVertexSolutions tr
        in
            case quadToStandardSolSet' qs of
                 sscr ->
                    setEq expected (sscr_final sscr))
        


zeroeyNumbers :: (Num a, Arbitrary a) => Gen a
zeroeyNumbers = 
    frequency 
    [ (1,return 0)
    , (3,arbitrary)
    ]

vectorShrinkElt
  :: (Ord a, VG.Vector v a, Arbitrary a) => v a -> [v a]
vectorShrinkElt v = concatMap (vectorAdjustAtF v shrink) [0..VG.maxIndex v]

prop_sd_zeroSet :: Property
prop_sd_zeroSet =
    let maxn = 40 in 
    forAllShrink (choose (1,maxn)) (filter (>0) . shrink) (\n ->
    forAllShrink (V.fromList <$> vectorOf (7*n) (zeroeyNumbers :: Gen Integer)) vectorShrinkElt (\v ->
    printTestCase ("expected =\n"++reverse [ if v V.! i == 0 then '1' else '0' | i <- [0..7*n-1]]) $
    withBitVectorType (7*n) (\(p :: Proxy w) ->
    printTestCase (show (typeOf p)) $
    let zs = sd_zeroSet (sd_fromVector v) :: w in
    printTestCase ("actual =\n"++(reverse . take (7*n). reverse . bvShow) zs) $
    forAllShrink (choose (0,7*n-1)) shrink (\i ->
    bvUnsafeIndex zs i .=. ((V.!) v i == 0))))) 


polyprop_vertexSolutions
  :: (Show (AdmissibleFor c (WrappedVector c V.Vector Rational)),
      CheckAdmissibility
        c (AdmissibleFor c (WrappedVector c V.Vector Rational)) Rational,
      CoordSys c) =>
     Proxy c -> Int -> Property
polyprop_vertexSolutions c n =
    mapSize (min n) $ 

    \(ManifoldTriangulation (tr :: Triangulation)) ->
    let
        sols = vertexSolutions c tr
    in
        conjoin
            (map (eitherFuncToProp (admissible c tr)) (V.toList sols))

prop_qVertexSolutions = polyprop_vertexSolutions quadCoordSys 50
prop_sVertexSolutions = polyprop_vertexSolutions stdCoordSys 50

prop_qVertexSolutionExts =
    
    mapSize (min 50) $ 
    \(ManifoldTriangulation (tr :: Triangulation)) ->
                                                               
        let
            sols = qVertexSolutionExts tr
        in 
            conjoin
                (map (eitherFuncToProp (standard_admissible tr)) (V.toList sols))



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






prop_eulerCharViaConcrete :: Property
prop_eulerCharViaConcrete = mapSize (`div` 2) $
    \(FundamentalEdgeSolution (ManifoldTriangulation (tr :: Triangulation)) surf) ->

        let
            cverts = concreteCorners surf
            carcs = concreteArcs surf
            ctris = concreteTris surf
            cquads = concreteQuads surf

            distinctsAfterPMap :: (Ord a, TriangulationDSnakeItem a, Show a) => [a] -> Int
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
    property (isDisk (toAdmissible stdCoordSys tr (i ./ d)))



        


prop_partialCanonicalPart_idempotent (TriangulationWithUnrestrictedStandardCoords tr s) =
    forAllElements (vertices tr)
        (\v -> partialCanonicalPart v s .=. partialCanonicalPart v (partialCanonicalPart v s))

prop_partialCanonicalPart_commute (TriangulationWithUnrestrictedStandardCoords tr s) =
    join forAllElements2 (vertices tr)
        (\(v,w) -> 
            partialCanonicalPart v (partialCanonicalPart w s)
                .=.
            partialCanonicalPart w (partialCanonicalPart v s))


--prop_quad_admissible_no_change 



prop_edgeNeighborhoodTetStream1 tr s = 
    forAllElementsN (tOIEdges tr) "e" (\e -> p (edgeNeighborhoodTetStream tr s e))

    where
        p xxs@(x:|xs) = 
            
            printTestCase ("xxs = "++show xxs) $

            f (==x)
            .=. 
            f (\y -> ient_centralEdge x == ient_centralEdge y)

            where
                f breakPred =
                    second (take 1) (break breakPred xs)






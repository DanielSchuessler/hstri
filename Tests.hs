{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Tests(qc_Tests) where

--import QuadCoordinates.Class
import AbstractNeighborhood
import ClosedNorCensus8
import ClosedOrCensus6
import Control.Applicative
import Control.Monad.State
import Data.Function
import Data.List as L
import Data.Maybe
import Data.Ord
import Data.Vector(Vector)
import HomogenousTuples
import InnerProductRepresentation
import MathUtil
import PrettyUtil
import QuadCoordinates
import QuadCoordinates.CanonExt
import QuadCoordinates.MatchingEquations
import QuickCheckUtil
import StandardCoordinates
import StandardCoordinates.MatchingEquations
import Test.QuickCheck
import Test.QuickCheck.All
import Triangulation.Random()
import Triangulation.VertexLink
import TriangulationCxtObject
import Util
import VectorUtil
import VerboseDD
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

eitherFuncToProp f x =
    printTestCase (show x) $
    either (\e -> printTestCase e False) (const (property True)) (f x)

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
    forAllElements closedOrCensus6 isManifoldTriangulation
    .&.
    forAllElements closedNorCensus8 isManifoldTriangulation

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
            krB = krBasis t :: [StandardCoordinates Int]
            mE = matchingEquations t
         in
            conjoin' [ (x <.> y) == 0 
                        | x <- krB
                        , y <- mE ])


{-# LANGUAGE MultiParamTypeClasses, TupleSections, ScopedTypeVariables, DeriveFunctor, TypeFamilies, StandaloneDeriving, GeneralizedNewtypeDeriving, ImplicitParams, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}
module StandardCoordinates(
    module Data.VectorSpace,
    module StandardCoordinates.Class,
    StandardCoordinates,
    stc_toMap,
    stc_fromMap,
    stc_fromDenseList,
    stc_toAssocs,
    stc_fromAssocs,
    -- * Properties
    stc_coefficient,
    stc_coefficientIsZero,
    stc_set,
    normalArcCounts,
    standard_admissible,
    satisfiesMatchingEquations,
    satisfiesQuadrilateralConstraints,

    -- * Matching equations
    MatchingEquationReason(..),
    matchingEquations,
    matchingEquationsWithReasons,
    explainMatchingEquations,

    -- * Vertex solutions
    getVertexSolutions,
    getVertexSolutions',
    toFundamentalEdgeSurface,
    getFundamentalEdgeSurfaces,

    -- * Kang-Rubinstein basis
    mkTetrahedralSolution,
    mkEdgeSolution,
    krBasis,



    ) where

import Tetrahedron as Abstract
import Control.Arrow
import Control.Exception
import Control.Monad.Reader
import Data.AdditiveGroup hiding(Sum)
import Data.Functor
import Data.Map
import Data.Maybe
import Data.Ratio
import Data.VectorSpace hiding(Sum)
import HomogenousTuples
import INormalDisc
import MathUtil
import PolymakeInterface
import Prelude hiding(lookup)
import Quote
import Test.QuickCheck
import Triangulation
import TriangulationCxtObject
import Math.SparseVector
import qualified Data.List as L
import qualified Data.Map as M
import Data.SumType
import StandardCoordinates.Class
import StandardCoordinates.MatchingEquations
import QuadCoordinates.MatchingEquations

-- Invariant: No value of the map is zero; zero coefficients are represented by the basis vector being absent from the map 

type StandardCoordinates r = SparseVector INormalDisc r
    
stc_toZDM = id
stc_fromZDM = id


stc_toMap :: StandardCoordinates r -> Map INormalDisc r
stc_toMap = illdefinedSparseToMap . stc_toZDM

stc_fromMap :: Num r => Map INormalDisc r -> StandardCoordinates r
stc_fromMap = stc_fromZDM . sparse_fromMap

-- instance Num r => VectorSpace (StandardCoordinates r) where 
--     type Scalar (StandardCoordinates r) = r
--     r *^ (stc_toZDM -> x) = stc_fromZDM (r *^ x)


stc_map
  :: Num r' =>
     (r -> r') -> StandardCoordinates r -> StandardCoordinates r'
stc_map f = stc_fromZDM . sparse_map f . stc_toZDM




--deriving instance (Pretty r) => Pretty (StandardCoordinates r)


-- instance (Pretty r) => Show (StandardCoordinates r) where
--     showsPrec = prettyShowsPrec






mkTetrahedralSolution ti = 
    sumV (fmap standardAsSparse (normalTriList ti)
          ++
          fmap (negateV . standardAsSparse) (normalQuadList ti))

mkEdgeSolution e =
        sumV (f <$> equivalentIEdges e)
    where
        f preimage = (t1 ^+^ t2) ^-^ q
            where
                (t1,t2) = map2 standardAsSparse (normalTris preimage)
                q = standardAsSparse (iNormalQuadByDisjointEdge (forgetVertexOrder preimage))

-- | Construct the Kang-Rubinstein basis for the solution space of the matching equations of tindex triangulation
krBasis t = (mkTetrahedralSolution <$> tTetrahedra_ t) ++ (mkEdgeSolution <$> (edges t))


stc_coefficient
  :: (Num r, MakeINormalDisc a) => StandardCoordinates r -> a -> r
stc_coefficient (stc_toZDM -> sc) = sparse_get sc . iNormalDisc


-- instance (Ord i, Num i) => QuadCoords (StandardCoordinates i) i where
--     quadCount = defaultQuadCount
--     quadAssocs = defaultQuadAssocs
-- 
-- instance (Ord i, Num i) => StandardCoords (StandardCoordinates i) i where
--     discCount = stc_coefficient
--     discAssocs = stc_toAssocs

stc_coefficientIsZero
  :: (Num r, MakeINormalDisc a) =>
     StandardCoordinates r -> a -> Bool
stc_coefficientIsZero (stc_toZDM -> sc) = sparse_isZero sc . iNormalDisc





-- type StandardCoordinateFunctional tindex r = StandardCoordinates tindex r -> r










getVertexSolutions :: forall s. PmScalar s => Triangulation -> IO [[s]]
getVertexSolutions t = do 
    let k = triangTetCount t
        matchingEquations_ = fmap ( (0 :) . fmap fromInteger . ns_toDenseList t ) (matchingEquations t)

    liftIO (putStrLn ("matching equations = "++show matchingEquations_))
    
    let
        nonNegativityConditions :: [[s]]
        nonNegativityConditions = 
            [ 0 : fmap fromInteger (ns_toDenseList t (standardAsSparse x)) | x <- tINormalDiscs t ]

        sumOne :: [s]
        sumOne =
            ( (-1) : replicate (7 * k) 1 )

    polymakeGetVertices' nonNegativityConditions (sumOne : matchingEquations_)


stc_fromDenseList
  :: Num r => Triangulation -> [r] -> StandardCoordinates r
stc_fromDenseList tr = sparse_fromAssocs . zip (tINormalDiscs tr)


getVertexSolutions' :: PmScalar s => Triangulation -> IO [StandardCoordinates s]
getVertexSolutions' t = do
    s0 <- getVertexSolutions t

    let fromVec (r:rs) = assert (r==1) (stc_fromDenseList t rs) 
        fromVec _ = assert False undefined

    return (fromVec <$> s0)


getFundamentalEdgeSurfaces
  :: Triangulation -> IO [StandardCoordinates Integer]
getFundamentalEdgeSurfaces tr =
    fmap 
        (L.filter (isRight . satisfiesQuadrilateralConstraints tr) 
            . fmap toFundamentalEdgeSurface)
        (getVertexSolutions' tr :: IO [ StandardCoordinates Rational ])


stc_toAssocs :: StandardCoordinates r -> [(INormalDisc, r)]
stc_toAssocs = sparse_toAssocs . stc_toZDM

stc_fromAssocs :: Num r => [(INormalDisc, r)] -> StandardCoordinates r
stc_fromAssocs = stc_fromZDM . sparse_fromAssocs


toFundamentalEdgeSurface :: Integral i => StandardCoordinates (Ratio i) -> StandardCoordinates i
toFundamentalEdgeSurface stc =
    let
        l = lcms (fmap (denominator . snd) (stc_toAssocs stc))
    in
        stc_map (fromJust . ratioToIntegral . (*(l%1))) stc


--edgesOfNormalDisc :: NormalDics -> [

normalArcCounts
  :: Num a => StandardCoordinates a -> M.Map (I NormalArc) a
normalArcCounts (stc_toZDM -> m) = 
    M.fromListWith (+)
    . concatMap (\(d,r) -> fmap (,r) (normalArcList d)) 
    . sparse_toAssocs
    $ m



instance (Num r, Arbitrary r) => Arbitrary (StandardCoordinates r) where
    arbitrary = stc_fromAssocs <$> arbitrary 


--prop_normalArcCounts_welldefined ::


--     let
--         e = normalCornerGetContainingEdge e
--         (t,_) = triangles e
--         arcTypes = map2 (normalArcByTriangleAndVertex t) (vertices e)

                                          




instance Quote r => Quote (StandardCoordinates r) where
    quotePrec prec x =
        quoteParen (prec > 10)
            (quoteApp "stc_fromAssocs" (stc_toAssocs x))





stc_set
  :: (Num r, MakeINormalDisc a) =>
     a -> r -> StandardCoordinates r -> StandardCoordinates r
stc_set d r (stc_toZDM -> m) = stc_fromZDM (sparse_set (iNormalDisc d) r m)

-- | Identified through the standard inner product
type StandardCoordinateFunctional r = StandardCoordinates r 



matchingEquationReasonToVector
  :: Num r => MatchingEquationReason -> StandardCoordinateFunctional r
matchingEquationReasonToVector me =
    sparse_fromAssocs (fmap (id &&& (fromInteger . evalMatchingEquation me))
                            (asList (matchingEquationSupportDiscs me)))

matchingEquations ::  Num r => Triangulation -> [StandardCoordinateFunctional r]
matchingEquations = liftM (fmap matchingEquationReasonToVector) matchingEquationReasons

matchingEquationsWithReasons :: Num r => Triangulation -> 
    [(StandardCoordinates r, MatchingEquationReason)]
matchingEquationsWithReasons =
    fmap (matchingEquationReasonToVector &&& id) . matchingEquationReasons

explainMatchingEquations ::  Triangulation -> IO ()
explainMatchingEquations t = putStrLn $
    unlines (concatMap (\(x,y) -> [show (x :: StandardCoordinateFunctional Double), 
                                   "\t"++show y]) 
                       (matchingEquationsWithReasons t)) 
 

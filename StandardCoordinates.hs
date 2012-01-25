{-# LANGUAGE MultiParamTypeClasses, TupleSections, ScopedTypeVariables, DeriveFunctor, TypeFamilies, StandaloneDeriving, GeneralizedNewtypeDeriving, ImplicitParams, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module StandardCoordinates(
    module Data.VectorSpace,
    module StandardCoordinates.Class,
    StandardCoordinates,
    stc_toMap,
    stc_fromMap,
    stc_fromDenseList,
    stc_toAssocs,
    stc_fromAssocs,
    ToStandardCoordinates(..),
    -- * Properties
    stc_coefficient,
    stc_coefficientIsZero,
    stc_set,
    normalArcCounts,
    admissible,
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

import AbstractTetrahedron as Abstract
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
import IndexedSimplices
import MathUtil
import PolymakeInterface
import Prelude hiding(lookup)
import PrettyUtil
import Quote
import Test.QuickCheck
import Triangulation
import TriangulationCxtObject
import ZeroDefaultMap
import qualified Data.List as L
import qualified Data.Map as M
import Data.SumType
import StandardCoordinates.Class
import StandardCoordinates.MatchingEquations
import QuadCoordinates.Class

-- Invariant: No value of the map is zero; zero coefficients are represented by the basis vector being absent from the map 

newtype StandardCoordinates r = SC { stc_toZDM :: ZeroDefaultMap INormalDisc r } 
    deriving(AdditiveGroup,InnerSpace)

stc_toMap :: StandardCoordinates r -> Map INormalDisc r
stc_toMap = illdefinedZdmToMap . stc_toZDM

stc_fromMap :: Num r => Map INormalDisc r -> StandardCoordinates r
stc_fromMap = SC . zdm_fromMap

instance Num r => VectorSpace (StandardCoordinates r) where 
    type Scalar (StandardCoordinates r) = r
    r *^ SC x = SC (r *^ x)


stc_map
  :: Num r' =>
     (r -> r') -> StandardCoordinates r -> StandardCoordinates r'
stc_map f = SC . zdm_map f . stc_toZDM




--deriving instance (Pretty r) => Pretty (StandardCoordinates r)

instance Pretty r => Pretty (StandardCoordinates r) where
    pretty sc = pretty (stc_toAssocs sc) 

instance (Pretty r) => Show (StandardCoordinates r) where
    showsPrec = prettyShowsPrec

class ToStandardCoordinates a where
    standardCoordinates :: Num r => a -> StandardCoordinates r

instance ToStandardCoordinates INormalDisc where
    standardCoordinates x = SC (zdm_singleton x 1)

instance ToStandardCoordinates INormalTri where
    standardCoordinates = standardCoordinates . iNormalDisc

instance ToStandardCoordinates INormalQuad where
    standardCoordinates = standardCoordinates . iNormalDisc

mkNormalTri :: (Num r) => IVertex -> StandardCoordinates r
mkNormalTri = standardCoordinates . iNormalTri



mkNormalQuadByVertexAndTTriangle :: (Num r) => Vertex -> ITriangle -> StandardCoordinates r
mkNormalQuadByVertexAndTTriangle v t =
    standardCoordinates (iNormalQuadByVertexAndITriangle v t)

mkTetrahedralSolution :: (Num r) => TIndex -> StandardCoordinates r
mkTetrahedralSolution ti = 
    sumV (fmap standardCoordinates (normalTriList ti)
          ++
          fmap (negateV . standardCoordinates) (normalQuadList ti))

mkEdgeSolution :: (Num r) => TEdge -> StandardCoordinates r
mkEdgeSolution e =
        sumV (f <$> equivalentIEdges e)
    where
        f preimage = (t1 ^+^ t2) ^-^ q
            where
                (t1,t2) = map2 standardCoordinates (normalTris preimage)
                q = standardCoordinates (iNormalQuadByDisjointEdge (forgetVertexOrder preimage))

-- | Construct the Kang-Rubinstein basis for the solution space of the matching equations of tindex triangulation
krBasis :: Num r => Triangulation -> [StandardCoordinates r]
krBasis t = (mkTetrahedralSolution <$> tTetrahedra_ t) ++ (mkEdgeSolution <$> (edges t))


stc_coefficient
  :: (Num r, MakeINormalDisc a) => StandardCoordinates r -> a -> r
stc_coefficient (SC sc) = zdm_get sc . iNormalDisc


instance Num i => QuadCoords (StandardCoordinates i) i where
    quadCount = defaultQuadCount
    quadAssocs = defaultQuadAssocs

instance Num i => StandardCoords (StandardCoordinates i) i where
    discCount = stc_coefficient
    discAssocs = stc_toAssocs

stc_coefficientIsZero
  :: (Num r, MakeINormalDisc a) =>
     StandardCoordinates r -> a -> Bool
stc_coefficientIsZero (SC sc) = zdm_isZero sc . iNormalDisc





-- type StandardCoordinateFunctional tindex r = StandardCoordinates tindex r -> r










getVertexSolutions :: forall s. PmScalar s => Triangulation -> IO [[s]]
getVertexSolutions t = do 
    let k = triangTetCount t
        matchingEquations_ = fmap ( (0 :) . ns_toDenseList t ) (matchingEquations t)

    liftIO (putStrLn ("matching equations = "++show matchingEquations_))
    
    let
        nonNegativityConditions :: [[s]]
        nonNegativityConditions = 
            [ 0 : ns_toDenseList t (standardCoordinates x) | x <- tINormalDiscs t ]

        sumOne :: [s]
        sumOne =
            ( (-1) : replicate (7 * k) 1 )

    polymakeGetVertices' nonNegativityConditions (sumOne : matchingEquations_)


stc_fromDenseList
  :: Num r => Triangulation -> [r] -> StandardCoordinates r
stc_fromDenseList t = sumV . zipWith f (tINormalDiscs t)
    where
        f nd r = r *^ standardCoordinates nd 


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
stc_toAssocs = zdm_toAssocs . stc_toZDM

stc_fromAssocs :: Num r => [(INormalDisc, r)] -> StandardCoordinates r
stc_fromAssocs = SC . zdm_fromAssocs


toFundamentalEdgeSurface :: Integral i => StandardCoordinates (Ratio i) -> StandardCoordinates i
toFundamentalEdgeSurface stc =
    let
        l = lcms (fmap (denominator . snd) (stc_toAssocs stc))
    in
        stc_map (fromJust . ratioToIntegral . (*(l%1))) stc


--edgesOfNormalDisc :: NormalDics -> [

normalArcCounts
  :: Num a => StandardCoordinates a -> M.Map (I NormalArc) a
normalArcCounts (SC m) = 
    M.fromListWith (+)
    . concatMap (\(d,r) -> fmap (,r) (normalArcList d)) 
    . zdm_toAssocs
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


-- | Sum of the coordinates of the elements
instance ToStandardCoordinates a => ToStandardCoordinates [a] where
    standardCoordinates xs = sumV (fmap standardCoordinates xs)



stc_set
  :: (Num r, MakeINormalDisc a) =>
     a -> r -> StandardCoordinates r -> StandardCoordinates r
stc_set d r (SC m) = SC (zdm_set (iNormalDisc d) r m)

-- | Identified through the standard inner product
type StandardCoordinateFunctional r = StandardCoordinates r 

matchingEquationReasonToVector
  :: Num r => MatchingEquationReason -> StandardCoordinateFunctional r
matchingEquationReasonToVector (MatchingEquationReason x x' v v') =
                              (mkNormalTri (getTIndex x ./ v)
                          ^+^ mkNormalQuadByVertexAndTTriangle v x)
                          ^-^ mkNormalTri (getTIndex x' ./ v')
                          ^-^ mkNormalQuadByVertexAndTTriangle v' (forgetVertexOrder x')

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



{-# LANGUAGE MultiParamTypeClasses, TupleSections, ScopedTypeVariables, DeriveFunctor, TypeFamilies, StandaloneDeriving, GeneralizedNewtypeDeriving, ImplicitParams, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module StandardCoordinates(
    module Data.VectorSpace,
    module NormalSurface,
    StandardCoordinates,
    stc_toMap,
    stc_fromMap,
    stc_fromDenseList,
    stc_toAssocs,
    stc_fromAssocs,
    ToStandardCoordinates(..),
    vertexLinkingSurface,
    vertexLinkEulerC,
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
    matchingEquationsWithReasons,

    -- * Vertex solutions
    getVertexSolutions,
    getVertexSolutions',
    toFundamentalEdgeSurface,
    getFundamentalEdgeSurfaces,

    -- * Kang-Rubinstein basis
    mkTetrahedralSolution,
    mkEdgeSolution,
    krBasis,

    -- * Testing
    qc_StandardCoordinates


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
import NormalSurface
import PolymakeInterface
import Prelude hiding(lookup)
import PrettyUtil
import QuickCheckUtil
import Quote
import Test.QuickCheck
import Test.QuickCheck.All
import Triangulation
import TriangulationCxtObject
import ZeroDefaultMap
import qualified Data.Foldable as Fold
import qualified Data.List as L
import qualified Data.Map as M
import Data.SumType

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


mkNormalQuadByDisjointEdge :: (Num r) => TIndex -> Abstract.Edge -> StandardCoordinates r
mkNormalQuadByDisjointEdge ti e = standardCoordinates (ti ./ normalQuadByDisjointEdge e)

mkNormalQuadByVertexAndTTriangle :: (Num r) => Vertex -> ITriangle -> StandardCoordinates r
mkNormalQuadByVertexAndTTriangle v (viewI -> I i f) = 
    mkNormalQuadByDisjointEdge i (edgeByOppositeVertexAndTriangle v f)


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

instance Num i => NormalSurface (StandardCoordinates i) i where
    discCount = stc_coefficient
    nsToAssocs = stc_toAssocs

stc_coefficientIsZero
  :: (Num r, MakeINormalDisc a) =>
     StandardCoordinates r -> a -> Bool
stc_coefficientIsZero (SC sc) = zdm_isZero sc . iNormalDisc





-- type StandardCoordinateFunctional tindex r = StandardCoordinates tindex r -> r

-- | Identified through the standard inner product
type StandardCoordinateFunctional r = StandardCoordinates r 

matchingEquations ::  Num r => Triangulation -> [StandardCoordinateFunctional r]
matchingEquations = liftM (fmap matchingEquationReasonToVector) matchingEquationReasons

data MatchingEquationReason = MatchingEquationReason ITriangle OITriangle Vertex Vertex

instance Show MatchingEquationReason where
    show (MatchingEquationReason x x' v v')
        = unwords [ "Gluing", show x, "to", show x', 
                    "identifies the normal arcs",
                    show (iNormalArc (x, v)),
                    "and",
                    show (iNormalArc (x', v'))
                  ]
                            
matchingEquationReasons
  :: Triangulation -> [MatchingEquationReason]
matchingEquationReasons t =
      [
         MatchingEquationReason x x' (forgetTIndex v) (forgetTIndex v')
               
        |
            (x,x') <- tOriginalGluings t,
            (v,v') <- zip (vertexList x) (vertexList x')

            ]

matchingEquationReasonToVector
  :: Num r => MatchingEquationReason -> StandardCoordinateFunctional r
matchingEquationReasonToVector (MatchingEquationReason x x' v v') =
                              (mkNormalTri (getTIndex x ./ v)
                          ^+^ mkNormalQuadByVertexAndTTriangle v x)
                          ^-^ mkNormalTri (getTIndex x' ./ v')
                          ^-^ mkNormalQuadByVertexAndTTriangle v' (forgetVertexOrder x')

matchingEquationsWithReasons :: Num r => Triangulation -> 
    [(StandardCoordinates r, MatchingEquationReason)]
matchingEquationsWithReasons =
    fmap (matchingEquationReasonToVector &&& id) . matchingEquationReasons
    


explainMatchingEquations ::  Triangulation -> IO ()
explainMatchingEquations t = putStrLn $
    unlines (concatMap (\(x,y) -> [show (x :: StandardCoordinateFunctional Double), 
                                   "\t"++show y]) 
                       (matchingEquationsWithReasons t)) 



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




qc_StandardCoordinates ::  IO Bool
qc_StandardCoordinates = $(quickCheckAll)


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
        (L.filter (isRight . satisfiesQuadrilateralConstraints (tTetrahedra_ tr)) 
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

                                          

admissible
  :: (Num r, Ord r) => Triangulation -> StandardCoordinates r -> Either String ()
admissible tr stc@(SC m) = do
    Fold.mapM_ (\r -> unless (r >= 0) (Left ("Negative coefficient"))) m
    satisfiesQuadrilateralConstraints (tTetrahedra_ tr) stc 
    satisfiesMatchingEquations tr stc

satisfiesMatchingEquations
  :: Num r =>
     Triangulation -> StandardCoordinates r -> Either [Char] ()
satisfiesMatchingEquations tr stc =
        mapM_ p (matchingEquationReasons tr)
    where
        p me = unless (r==0)
                      (Left ("Matching equation not satisfied: "++show me++" (LHS: "++show r++")"))
            where
                r = matchingEquationReasonToVector me <.> stc

satisfiesQuadrilateralConstraints
  :: (Num r, Ord r) =>
     [TIndex] -> StandardCoordinates r -> Either String ()
satisfiesQuadrilateralConstraints tets stc = 
        mapM_ p tets
    where
        p tet = 
            unless (sum3 (map3 f (normalQuads tet)) <= (1::Int))
                   (Left ("Quadrilateral constraints violated at tet "++show tet))

        f quad = if stc_coefficient stc (iNormalDisc quad) == 0 
                    then 0
                    else 1



instance Quote r => Quote (StandardCoordinates r) where
    quotePrec prec x =
        quoteParen (prec > 10)
            (quoteApp "stc_fromAssocs" (stc_toAssocs x))


-- | Sum of the coordinates of the elements
instance ToStandardCoordinates a => ToStandardCoordinates [a] where
    standardCoordinates xs = sumV (fmap standardCoordinates xs)


vertexLinkingSurface :: TVertex -> StandardCoordinates Integer
vertexLinkingSurface = standardCoordinates . vertexLinkingSurfaceTris

vertexLinkEulerC :: TVertex -> Rational
vertexLinkEulerC v = eulerCRatio (getTriangulation v) . vertexLinkingSurface $ v 

stc_set
  :: (Num r, MakeINormalDisc a) =>
     a -> r -> StandardCoordinates r -> StandardCoordinates r
stc_set d r (SC m) = SC (zdm_set (iNormalDisc d) r m)



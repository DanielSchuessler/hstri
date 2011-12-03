{-# LANGUAGE TupleSections, ScopedTypeVariables, DeriveFunctor, TypeFamilies, StandaloneDeriving, GeneralizedNewtypeDeriving, ImplicitParams, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module NormalCoordinates where

import AbstractTetrahedron as Abstract
import Collections
import Control.Arrow
import Control.Exception
import Control.Monad.Reader
import Data.AdditiveGroup
import Data.Functor
import Data.Monoid
import Data.VectorSpace
import HomogenousTuples
import INormalDisc
import IndexedSimplices
import PolymakeInterface
import Prelude hiding(lookup)
import PrettyUtil
import Test.QuickCheck
import Test.QuickCheck.All
import Triangulation
import TriangulationCxtObject
import qualified Data.Foldable as Fold
import qualified Data.Map as M

-- Invariant: No value of the map is zero; zero coefficients are represented by the basis vector being absent from the map 

newtype StandardCoordinates r = SC (Map INormalDisc r) 

deriving instance Functor StandardCoordinates

instance (Num r) => AdditiveGroup (StandardCoordinates r) where
    zeroV = SC mempty
    SC xs ^+^ SC ys = SC (unionMaybeWith f xs ys)
        where
            f x y = case x + y of
                         0 -> Nothing
                         s -> Just s
    negateV = fmap negate

instance (Num r) => VectorSpace (StandardCoordinates r) where
    type Scalar (StandardCoordinates r) = r
    0 *^ _ = zeroV
    r *^ x = fmap (r*) x 

instance (Num r) => InnerSpace (StandardCoordinates r) where
    SC x <.> SC y = Fold.foldl' (+) 0 (intersectionWith (*) x y)

deriving instance (Pretty r) => Pretty (StandardCoordinates r)

instance (Pretty r) => Show (StandardCoordinates r) where
    show = prettyString

class NormalSurface a where
    standardCoordinates :: Num r => a -> StandardCoordinates r

instance NormalSurface INormalDisc where
    standardCoordinates x = SC (singletonMap x 1)

instance NormalSurface INormalTri where
    standardCoordinates = standardCoordinates . iNormalDisc

instance NormalSurface INormalQuad where
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
                q = standardCoordinates (iNormalQuadByDisjointEdge preimage)

-- | Construct the Kang-Rubinstein basis for the solution space of the matching equations of tindex triangulation
krBasis :: Num r => Triangulation -> [StandardCoordinates r]
krBasis t = (mkTetrahedralSolution <$> tTetrahedra_ t) ++ (mkEdgeSolution <$> (edges t))


ncCoefficient :: (Num r) => StandardCoordinates r -> INormalDisc -> r
ncCoefficient (SC nc) nd = case lookup nd nc of
                                    Just r -> r
                                    Nothing -> 0


toVec :: (Num r) => Triangulation -> StandardCoordinates r -> [r] 
toVec t nc = fmap (ncCoefficient nc) (allINormalDiscs t)

allINormalDiscs :: Triangulation -> [INormalDisc]
allINormalDiscs t = [ ti ./ ndt | ti <- (tTetrahedra_ t) , ndt <- allNormalDiscs ]




-- type StandardCoordinateFunctional tindex r = StandardCoordinates tindex r -> r

-- | Identified through the standard inner product
type StandardCoordinateFunctional r = StandardCoordinates r 

matchingEquations ::  Num r => Triangulation -> [StandardCoordinates r]
matchingEquations = liftM (fmap fst) matchingEquationsWithReasons

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
            (x,x') <- tGluings_ t,
            (v,v') <- zip (vertexList x) (vertexList x')

            ]

matchingEquationReasonToVector
  :: Num r => MatchingEquationReason -> StandardCoordinates r
matchingEquationReasonToVector (MatchingEquationReason x x' v v') =
                              (mkNormalTri (getTIndex x ./ v)
                          ^+^ mkNormalQuadByVertexAndTTriangle v x)
                          ^-^ mkNormalTri (getTIndex x' ./ v')
                          ^-^ mkNormalQuadByVertexAndTTriangle v' (forgetVertexOrder x')

matchingEquationsWithReasons :: Num r => Triangulation -> [(StandardCoordinates r, MatchingEquationReason)]
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
            conjoin [ (x <.> y) == 0 
                        | x <- krB
                        , y <- mE ])




qc_NormalCoordinates ::  IO Bool
qc_NormalCoordinates = $(quickCheckAll)


getVertexSolutions :: forall s. PmScalar s => Triangulation -> IO [[s]]
getVertexSolutions t = do 
    let k = triangTetCount t
        matchingEquations_ = fmap ( (0 :) . toVec t ) (matchingEquations t)

    liftIO (putStrLn ("matching equations = "++show matchingEquations_))
    
    let
        nonNegativityConditions :: [[s]]
        nonNegativityConditions = 
            [ 0 : toVec t (standardCoordinates x) | x <- allINormalDiscs t ]

        sumOne :: [s]
        sumOne =
            ( (-1) : replicate (7 * k) 1 )

    polymakeGetVertices' nonNegativityConditions (sumOne : matchingEquations_)




getVertexSolutions' :: PmScalar s => Triangulation -> IO [StandardCoordinates s]
getVertexSolutions' t = do
    s0 <- getVertexSolutions t

    let fromVec (r:rs) = assert (r==1) . sumV $ zipWith f rs (allINormalDiscs t)
        fromVec _ = assert False undefined
        f r nd = r *^ standardCoordinates nd 

    return (fromVec <$> s0)


--edgesOfNormalDisc :: NormalDics -> [

normalArcCounts
  :: Num a => StandardCoordinates a -> M.Map (I NormalArc) a
normalArcCounts (SC m) = 
    M.fromListWith (+)
    . concatMap (\(d,r) -> fmap (,r) (normalArcList d)) 
    . M.toList
    $ m

instance (Num r, Arbitrary r) => Arbitrary (StandardCoordinates r) where
    arbitrary = 
        (SC . M.fromListWith (+)) <$> arbitrary 


--prop_normalArcCounts_welldefined ::



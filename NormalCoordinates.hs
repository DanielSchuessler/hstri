{-# LANGUAGE ScopedTypeVariables, DeriveFunctor, TypeFamilies, StandaloneDeriving, GeneralizedNewtypeDeriving, ImplicitParams, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleContexts #-}
{-# OPTIONS -Wall #-}
module NormalCoordinates where

import AbstractTetrahedron as Abstract
import Collections
import Control.Exception
import Control.Monad.Reader
import Data.AdditiveGroup
import Data.Functor
import Data.Monoid
import Data.VectorSpace
import IndexedSimplices
import PolymakeInterface
import Test.QuickCheck
import Test.QuickCheck.All
import Text.PrettyPrint.ANSI.Leijen hiding((<$>))
import Triangulation
import TriangulationCxtObject
import qualified Data.Foldable as Fold
import Prelude hiding(lookup)

-- Invariant: No value of the map is zero; zero coefficients are represented by the basis vector being absent from the map 

newtype NormalCoordinate r = NC (Map INormalDisc r) 

deriving instance Functor NormalCoordinate

instance (Num r) => AdditiveGroup (NormalCoordinate r) where
    zeroV = NC mempty
    NC xs ^+^ NC ys = NC (unionMaybeWith f xs ys)
        where
            f x y = case x + y of
                         0 -> Nothing
                         s -> Just s
    negateV = fmap negate

instance (Num r) => VectorSpace (NormalCoordinate r) where
    type Scalar (NormalCoordinate r) = r
    0 *^ _ = zeroV
    r *^ x = fmap (r*) x 

instance (Num r) => InnerSpace (NormalCoordinate r) where
    NC x <.> NC y = Fold.foldl' (+) 0 (intersectionWith (*) x y)

deriving instance (Pretty r) => Pretty (NormalCoordinate r)

instance (Pretty r) => Show (NormalCoordinate r) where
    show = prettyString

toNormalCoordinate :: (Num r) => INormalDisc -> NormalCoordinate r
toNormalCoordinate x = NC (singletonMap x 1)

mkNormalTri :: (Num r) => IVertex -> NormalCoordinate r
mkNormalTri (viewI -> I i v) = 
    toNormalCoordinate (i ./ (normalDisc (normalTri v))) 


mkNormalQuadByQuad :: (Num r) => TIndex -> NormalQuad -> NormalCoordinate r
mkNormalQuadByQuad ti q = toNormalCoordinate (ti ./ (normalDisc q))

mkNormalQuadByDisjointEdge :: (Num r) => TIndex -> Abstract.Edge -> NormalCoordinate r
mkNormalQuadByDisjointEdge ti e = toNormalCoordinate (ti ./ (normalDisc $ normalQuadByDisjointEdge e))

mkNormalQuadByVertexAndTTriangle :: (Num r) => Vertex -> ITriangle -> NormalCoordinate r
mkNormalQuadByVertexAndTTriangle v (viewI -> I i f) = 
    mkNormalQuadByDisjointEdge i (edgeByOppositeVertexAndTriangle v f)


mkTetrahedralSolution :: (Num r) => TIndex -> NormalCoordinate r
mkTetrahedralSolution ti = 
    sumV (((mkNormalTri . (ti ./)) <$> Abstract.allVertices)
          ++
          ((negateV . mkNormalQuadByQuad) ti <$> [Q,Q',Q'']))

mkEdgeSolution :: (Num r) => TEdge -> NormalCoordinate r
mkEdgeSolution e =
        sumV (f <$> equivalentIEdges e)
    where
        f preimage = (t1 ^+^ t2) ^-^ q
            where
                ti = getTIndex preimage
                ae = forgetTIndex preimage
                avs = vertices ae
                t1 = mkNormalTri (ti ./ fst avs)
                t2 = mkNormalTri (ti ./ snd avs)
                q = mkNormalQuadByDisjointEdge ti ae

-- | Construct the Kang-Rubinstein basis for the solution space of the matching equations of tindex triangulation
krBasis :: Num r => Triangulation -> [NormalCoordinate r]
krBasis t = (mkTetrahedralSolution <$> tTetrahedra_ t) ++ (mkEdgeSolution <$> (tEdges t))


ncCoefficient :: (Num r) => NormalCoordinate r -> INormalDisc -> r
ncCoefficient (NC nc) nd = case lookup nd nc of
                                    Just r -> r
                                    Nothing -> 0

toVec :: (Num r) => Triangulation -> NormalCoordinate r -> [r] 
toVec t nc = fmap (ncCoefficient nc) (allINormalDiscs t)

allINormalDiscs :: Triangulation -> [INormalDisc]
allINormalDiscs t = [ ti ./ ndt | ti <- (tTetrahedra_ t) , ndt <- allNormalDiscs ]




-- type NormalCoordinateFunctional tindex r = NormalCoordinate tindex r -> r

-- | Identified through the standard inner product
type NormalCoordinateFunctional r = NormalCoordinate r 

matchingEquations ::  Num r => Triangulation -> [NormalCoordinate r]
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
                            

matchingEquationsWithReasons :: Num r => Triangulation -> [(NormalCoordinate r, MatchingEquationReason)]
matchingEquationsWithReasons t =
    
      [
            (y,y_reason)
               
        |
            (x,x') <- tGluings_ t,
            (v,v') <- zip (vertexList x) (vertexList x'),

            let y =  (((mkNormalTri v                                                                    
                          ^+^ mkNormalQuadByVertexAndTTriangle (forgetTIndex v) x)                       
                          ^-^ mkNormalTri v')                                                            
                          ^-^ mkNormalQuadByVertexAndTTriangle (forgetTIndex v') (forgetVertexOrder  x')),

            let y_reason = MatchingEquationReason x x' (forgetTIndex v) (forgetTIndex v')

            ]


explainMatchingEquations ::  Triangulation -> IO ()
explainMatchingEquations t = putStrLn $
    unlines (concatMap (\(x,y) -> [show (x :: NormalCoordinateFunctional Double), 
                                   "\t"++show y]) 
                       (matchingEquationsWithReasons t)) 



-- | Test that each element of krBasis satisfies the matching equations
prop_krBasisMatchingEquations ::  Triangulation -> Gen Prop
prop_krBasisMatchingEquations t =
        (let
            krB = krBasis t :: [NormalCoordinate Int]
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
            [ 0 : toVec t (toNormalCoordinate x) | x <- allINormalDiscs t ]

        sumOne :: [s]
        sumOne =
            ( (-1) : replicate (7 * k) 1 )

    polymakeGetVertices' nonNegativityConditions (sumOne : matchingEquations_)




getVertexSolutions' :: PmScalar s => Triangulation -> IO [NormalCoordinate s]
getVertexSolutions' t = do
    s0 <- getVertexSolutions t

    let fromVec (r:rs) = assert (r==1) . sumV $ zipWith f rs (allINormalDiscs t)
        fromVec _ = assert False undefined
        f r nd = r *^ toNormalCoordinate nd 

    return (fromVec <$> s0)


--edgesOfNormalDisc :: NormalDics -> [

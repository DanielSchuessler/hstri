{-# LANGUAGE FunctionalDependencies, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module QuadCoordinates.MatchingEquations where

import Triangulation.AbstractNeighborhood
import Control.Arrow((&&&))
import Control.DeepSeq.TH
import Control.Exception
import Control.Monad.State
import Data.Function
import Data.List as L
import Data.Maybe as May
import HomogenousTuples
import Tetrahedron.INormalDisc
import MathUtil
import PrettyUtil
import QuadCoordinates.Class
import Tetrahedron
import Triangulation.Class
import TriangulationCxtObject
import Control.Applicative




satisfiesQuadrilateralConstraints
  :: (Num r, QuadCoords s r) => Triangulation -> s -> Either [Char] ()
satisfiesQuadrilateralConstraints tr stc = 
        mapM_ p tets
    where
        tets = tTetrahedra_ tr
        
        p tet = 
            unless (sum3 (map3 f (normalQuads tet)) <= (1::Int))
                   (Left ("Quadrilateral constraints violated at tet "++show tet))

        f quad = if quadCount stc quad == 0 
                    then 0
                    else 1



quad_admissible
  :: (Show r, ToTriangulation tr, QuadCoords q r) =>
     tr -> q -> Either [Char] (QAdmissible q)
quad_admissible (toTriangulation -> tr) qc = do
    mapM_ (\r -> unless (snd r >= 0) (Left ("Negative coefficient"))) (quadAssocs qc) 
    satisfiesQuadrilateralConstraints tr qc 
    satisfiesQMatchingEquations tr qc
    return (UnsafeToQAdmissible tr qc)

evalQMatchingEquation
  :: QuadCoords q b => QMatchingEquation -> q -> b
evalQMatchingEquation (me :: QMatchingEquation) qc =
    foldl' (+) 0 
    . L.map (\(_,(u,d)) -> quadCount qc u - quadCount qc d) 
    $ me


satisfiesQMatchingEquations
  :: (Show r, QuadCoords q r) => Triangulation -> q -> Either [Char] ()
satisfiesQMatchingEquations tr qc =
        mapM_ p (qMatchingEquations0 tr)
    where
        p me = unless (r==0)
                      (Left ("Matching equation not satisfied: "++show me++" (LHS: "++show r++")"))
            where
                r = evalQMatchingEquation me qc

type QMatchingEquation = [(IEdgeNeighborhoodTet, 
    (
    INormalQuad {- upwards sloped -}, 
    INormalQuad {- downwards sloped -}
    )
    )]

-- | The sign of the resulting equation is chosen arbitrarily.
qMatchingEquation0 :: TEdge -> Maybe QMatchingEquation
qMatchingEquation0 te = do
    triPairs <- innerEdgeNeighborhood' (getTriangulation te) (toOrderedFace . unT $ te)
    (return . fmap f ) triPairs
        
  where
    f = id &&&
        (\(viewI -> I i ent ) ->
            let 
                a = ent_top ent
                b = ent_bot ent
                c = ent_left ent
                q = curry ((i./) . normalQuadByDisjointEdge . edge)
            in
                (   q a c
                ,   q b c
                ))

qMatchingEquations0
  ::
     Triangulation
     -> [QMatchingEquation]
qMatchingEquations0 = mapMaybe qMatchingEquation0 . edges

-- | INVARIANT: @'quad_isAdmissible' ('qadm_Triangulation' x) ('qadm_coords' x) == True@.
data QAdmissible q = UnsafeToQAdmissible {
    -- | The triangulation with respect to which the 'qadm_coords' are admissible.
    qadm_Triangulation :: Triangulation,
    qadm_coords :: q 
}
    deriving(Show)

instance NormalSurfaceCoefficients q r => NormalSurfaceCoefficients (QAdmissible q) r

-- | Does /not/ compare the triangulations
instance Eq q => Eq (QAdmissible q) where
    (==) = (==) `on` qadm_coords

-- | Does /not/ compare the triangulations
instance Ord q => Ord (QAdmissible q) where
    compare = compare `on` qadm_coords

instance QuadCoords q r => QuadCoords (QAdmissible q) r where
    quadCount = quadCount . qadm_coords
    quadAssocs = quadAssocs . qadm_coords
    quadAssocsDistinct = quadAssocsDistinct . qadm_coords
    quadAsSparse = quadAsSparse . qadm_coords

unsafeToQAdmissible :: Triangulation -> q -> QAdmissible q
unsafeToQAdmissible = UnsafeToQAdmissible

instance Pretty q => Pretty (QAdmissible q) where
    prettyPrec prec = prettyPrec prec . qadm_coords


deriveNFData ''QAdmissible


qadm_unsafeMap :: (q1 -> q) -> QAdmissible q1 -> QAdmissible q
qadm_unsafeMap f q = q { qadm_coords = f (qadm_coords q) } 

instance (Num r, Ord r, NonNegScalable r q) => NonNegScalable r (QAdmissible q) where
    scaleNonNeg r = assert (r>=0) $ qadm_unsafeMap (scaleNonNeg r)

qadm_unsafeTraverse
  :: Functor f => (q1 -> f q) -> QAdmissible q1 -> f (QAdmissible q)
qadm_unsafeTraverse f q = (\x -> q { qadm_coords = x }) <$> f (qadm_coords q)

instance RatioToIntegral qr qi => RatioToIntegral (QAdmissible qr) (QAdmissible qi) where
    ratioToIntegral = qadm_unsafeTraverse ratioToIntegral

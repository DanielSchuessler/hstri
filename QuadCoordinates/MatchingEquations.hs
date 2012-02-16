{-# LANGUAGE DeriveDataTypeable, TypeFamilies, FunctionalDependencies, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module QuadCoordinates.MatchingEquations where

import Control.Applicative
import Control.Arrow((&&&))
import Control.DeepSeq
import Control.Exception
import Control.Monad.State
import Data.Function
import Data.List as L
import Data.Maybe as May
import Data.SumType
import Data.Typeable
import HomogenousTuples
import MathUtil
import PrettyUtil
import QuadCoordinates.Class
import Tetrahedron
import Triangulation.AbstractNeighborhood
import Triangulation.Class
import TriangulationCxtObject


data QuadConstraintViolation = QuadConstraintViolation TIndex
    deriving(Show,Typeable)

instance Exception QuadConstraintViolation

instance Pretty QuadConstraintViolation where 
    pretty (QuadConstraintViolation tet) = text "Quadrilateral constraints violated at tet "<>pretty tet


satisfiesQuadrilateralConstraints
  :: (Num r, QuadCoords s r) => Triangulation -> s -> EitherC QuadConstraintViolation ()
satisfiesQuadrilateralConstraints tr stc = 
        mapM_ p tets
    where
        tets = tTetrahedra_ tr
        
        p tet = 
            unless (sum3 (map3 f (normalQuads tet)) <= (1::Int))
                   (left' (QuadConstraintViolation tet))

        f quad = if quadCount stc quad == 0 
                    then 0
                    else 1


areCompatible
  :: (QuadCoords a r, QuadCoords b r) =>
     Triangulation -> a -> b -> EitherC QuadConstraintViolation ()
areCompatible tr x y =
    satisfiesQuadrilateralConstraints tr (x :+ y)

quad_admissible
  :: (Show q, Show r, ToTriangulation tr, QuadCoords q r) =>
     tr -> q -> AttemptC (QAdmissible q)
quad_admissible (toTriangulation -> tr) qc = do
    mapM_ (\r -> unless (snd r >= 0) 
                (toAttemptC $ $failureStr ("Negative coefficient"))) (quadAssocs qc) 
    toAttemptC $ satisfiesQuadrilateralConstraints tr qc 
    toAttemptC $ satisfiesQMatchingEquations tr qc
    return (UnsafeToQAdmissible tr qc)

evalQMatchingEquation
  :: QuadCoords q b => QMatchingEquation -> q -> b
evalQMatchingEquation (me :: QMatchingEquation) qc =
    foldl' (+) 0 
    . L.map (\(_,(u,d)) -> quadCount qc u - quadCount qc d) 
    $ me

type QMatchingEquation = [(IEdgeNeighborhoodTet, 
    (
    INormalQuad {- upwards sloped -}, 
    INormalQuad {- downwards sloped -}
    )
    )]

data QMatchingEquationViolation q = QMatchingEquationViolation {
        qmev_vector :: q,
        qmev_qme :: QMatchingEquation
    }
    deriving (Show,Typeable)

instance (Show q, Typeable q) => Exception (QMatchingEquationViolation q)

instance Pretty q => Pretty (QMatchingEquationViolation q) where
    pretty (QMatchingEquationViolation v me) = 
        text "Quad coordinate vector " <> pretty v <>
            text " doesn't satisfy the Q-matching equation " 
            <> pretty me


satisfiesQMatchingEquations
  :: (Show r, QuadCoords q r) => Triangulation -> q -> EitherC (QMatchingEquationViolation q) ()
satisfiesQMatchingEquations tr qc =
        mapM_ p (qMatchingEquations0 tr)
    where
        p me = unless (r==0)
                    (left' (QMatchingEquationViolation qc me))
            where
                r = evalQMatchingEquation me qc


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
data instance AdmissibleFor QuadCoordSys q = UnsafeToQAdmissible {
    -- | The triangulation with respect to which the 'qadm_coords' are admissible.
    qadm_Triangulation :: Triangulation,
    qadm_coords :: q 
}

type QAdmissible = AdmissibleFor QuadCoordSys

instance NFData q => NFData (QAdmissible q) where
    rnf (UnsafeToQAdmissible a b) = rnf a `seq` rnf b `seq` ()

instance Show q => Show (QAdmissible q) where
    showsPrec prec = showsPrec prec . qadm_coords


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




qadm_unsafeMap :: (q1 -> q) -> QAdmissible q1 -> QAdmissible q
qadm_unsafeMap f q = q { qadm_coords = f (qadm_coords q) } 

instance (Num r, Ord r, NonNegScalable r q) => NonNegScalable r (QAdmissible q) where
    scaleNonNeg r = assert (r>=0) $ qadm_unsafeMap (scaleNonNeg r)

qadm_unsafeTraverse
  :: Functor f => (q1 -> f q) -> QAdmissible q1 -> f (QAdmissible q)
qadm_unsafeTraverse f q = (\x -> q { qadm_coords = x }) <$> f (qadm_coords q)

instance RatioToIntegral qr qi => RatioToIntegral (QAdmissible qr) (QAdmissible qi) where
    ratioToIntegral = qadm_unsafeTraverse ratioToIntegral

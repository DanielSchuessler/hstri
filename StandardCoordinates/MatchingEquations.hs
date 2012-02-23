{-# LANGUAGE DeriveDataTypeable, TypeFamilies, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, ViewPatterns, TemplateHaskell, NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module StandardCoordinates.MatchingEquations where

import QuadCoordinates.MatchingEquations
import StandardCoordinates.Class
import QuadCoordinates.Class
import Control.Monad
import Triangulation
import TupleTH
import Triangulation.Class
import Data.Function
import PrettyUtil
import MathUtil
import Control.Exception
import Control.Applicative
import Control.DeepSeq
import Data.Typeable
import Data.SumType




standard_admissible
  :: (Show r, ToTriangulation tr, StandardCoords s r, Show s) =>
     tr -> s -> AttemptC (Admissible s)
standard_admissible (toTriangulation -> tr) stc = do
    mapM_ (\r -> unless (snd r >= 0) (toAttemptC $ $failureStr ("Negative coefficient")))
          (discAssocs stc)
    toAttemptC $ satisfiesQuadrilateralConstraints tr stc 
    toAttemptC $ satisfiesMatchingEquations tr stc
    return (UnsafeToAdmissible tr stc)

standard_toAdmissible
  :: (Show r, Show a, ToTriangulation tr, StandardCoords a r) =>
     tr -> a -> Admissible a
standard_toAdmissible tr = $unEitherC "standard_toAdmissible" . standard_admissible tr

data MatchingEquationViolation s = MatchingEquationViolation {
        mev_vector :: s,
        mev_me :: MatchingEquationReason
    }
    deriving (Show,Typeable)

instance (Show s, Typeable s) => Exception (MatchingEquationViolation s)


instance Pretty s => Pretty (MatchingEquationViolation s) where
    pretty (MatchingEquationViolation v me) = 
        text "Standard coordinate vector " <> pretty v <>
            text " doesn't satisfy the matching equation " 
            <> text (show me)



satisfiesMatchingEquations
  :: (Show r, StandardCoords s r) => Triangulation -> s -> EitherC (MatchingEquationViolation s) ()
satisfiesMatchingEquations tr stc =
        mapM_ p (matchingEquationReasons tr)
    where
        p me = unless (r==0)
                      (left' (MatchingEquationViolation stc me))
            where
                r = evalMatchingEquation me stc





data MatchingEquationReason = 
    MatchingEquationReason ITriangle OITriangle Vertex Vertex
        INormalTri INormalQuad INormalTri INormalQuad

instance Show MatchingEquationReason where
    show (MatchingEquationReason x x' v v' _ _ _ _)
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
                            (iNormalTri v)
                            (iNormalQuadByVertexAndITriangle (forgetTIndex v) x)
                            (iNormalTri v')
                            (iNormalQuadByVertexAndITriangle (forgetTIndex v') (forgetVertexOrder x'))
               
        |
            (x,x') <- tGluingsIrredundant t,
            (v,v') <- zip (vertexList x) (vertexList x')

            ]



evalMatchingEquation
  :: StandardCoords s r => MatchingEquationReason -> s -> r
evalMatchingEquation (MatchingEquationReason _ _ _ _ t1 q1 t2 q2) stc =
                            triCount stc t1
                          + quadCount stc q1

                          - triCount stc t2
                          - quadCount stc q2

    

matchingEquationSupport
  :: MatchingEquationReason
     -> (INormalTri, INormalQuad, INormalTri, INormalQuad)
matchingEquationSupport (MatchingEquationReason _ _ _ _ t1 q1 t2 q2) =
                        
                          (t1,q1,t2,q2)

matchingEquationSupportDiscs
  :: MatchingEquationReason
     -> (INormalDisc, INormalDisc, INormalDisc, INormalDisc)
matchingEquationSupportDiscs =
    $(mapTuple' 4 [|iNormalDisc|]) . matchingEquationSupport

-- | INVARIANT: @'isAdmissible' ('adm_Triangulation' x) ('adm_coords' x) == True@. 
data instance AdmissibleFor StdCoordSys s = UnsafeToAdmissible {
    -- | The triangulation with respect to which the 'adm_coords' are admissible.
    adm_Triangulation :: Triangulation,
    adm_coords :: s 
}

type Admissible = AdmissibleFor StdCoordSys

instance NFData q => NFData (Admissible q) where
    rnf (UnsafeToAdmissible a b) = rnf a `seq` rnf b `seq` ()

instance AsList s => AsList (Admissible s) where
    asList = asList . adm_coords


instance Show s => Show (Admissible s) where
    showsPrec prec = showsPrec prec . adm_coords

instance NormalSurfaceCoefficients q r => NormalSurfaceCoefficients (Admissible q) r

-- | Does /not/ compare the triangulations
instance Eq q => Eq (Admissible q) where
    (==) = (==) `on` adm_coords

-- | Does /not/ compare the triangulations
instance Ord q => Ord (Admissible q) where
    compare = compare `on` adm_coords

instance QuadCoords q r => QuadCoords (Admissible q) r where
    quadCount = quadCount . adm_coords
    quadAssocs = quadAssocs . adm_coords
    quadAssocsDistinct = quadAssocsDistinct . adm_coords
    quadAsSparse = quadAsSparse . adm_coords

instance StandardCoords s r => StandardCoords (Admissible s) r where
    discCount = discCount . adm_coords
    discAssocs = discAssocs . adm_coords
    discAssocsDistinct = discAssocsDistinct . adm_coords
    standardAsSparse = standardAsSparse . adm_coords

    triCount = triCount . adm_coords
    triAssocs = triAssocs . adm_coords
    triAssocsDistinct = triAssocsDistinct . adm_coords

unsafeToAdmissible :: Triangulation -> q -> Admissible q
unsafeToAdmissible = UnsafeToAdmissible

instance Pretty s => Pretty (Admissible s) where
    prettyPrec prec = prettyPrec prec . adm_coords


    


instance UpdatableStandardCoords s s' r => UpdatableStandardCoords (Admissible s) s' r
    where

    adjustTriCount f t = adjustTriCount f t . adm_coords
    adjustDiscCount f t = adjustDiscCount f t . adm_coords

adm_unsafeMap :: (s1 -> s) -> Admissible s1 -> Admissible s
adm_unsafeMap f s = s { adm_coords = f (adm_coords s) } 

instance (Num r, Ord r, NonNegScalable r s) => NonNegScalable r (Admissible s) where
    scaleNonNeg r = assert (r>=0) $ adm_unsafeMap (scaleNonNeg r)

adm_unsafeTraverse
  :: Functor f => (s1 -> f s) -> Admissible s1 -> f (Admissible s)
adm_unsafeTraverse f q = (\x -> q { adm_coords = x }) <$> f (adm_coords q)

instance RatioToIntegral qr qi => RatioToIntegral (Admissible qr) (Admissible qi) where
    ratioToIntegral = adm_unsafeTraverse ratioToIntegral




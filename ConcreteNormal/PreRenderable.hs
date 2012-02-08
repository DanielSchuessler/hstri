{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, CPP, RecordWildCards, NoMonomorphismRestriction, FlexibleInstances, StandaloneDeriving, GADTs, ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

module ConcreteNormal.PreRenderable(
    module SimplicialPartialQuotient,
    module PreRenderable,
    module Simplicial.SimplicialComplex,
    Corn,corn,cornVerts,
    normalSurfaceToPreRenderable,
    CornerPosition',CornerCount,

    ) where

import ConcreteNormal
import Data.Function
import Data.Set as Set
import Data.Vect.Double(interpolate)
import DisjointUnion
import HomogenousTuples
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import PreRenderable
import PrettyUtil
import ShortShow
import Simplicial.DeltaSet2
import Simplicial.SimplicialComplex
import SimplicialPartialQuotient
import StandardCoordinates.MatchingEquations
import TriangulationCxtObject
import Util

type CornerPosition' = Int
type CornerCount = Int

-- | An interior point of an edge of the 'SimplicialPartialQuotient'
data Corn v = Corn CornerPosition' CornerCount v v
                    -- INVARIANT: Third field <= fourth field
    deriving(Eq,Ord,Show)

cornVerts :: Corn t -> (t, t)
cornVerts (Corn _ _ u v) = (u,v)

instance ShortShow v => ShortShow (Corn v) where
    shortShow (Corn cp _ v0 v1) = shortShow (v0,v1) ++ show cp


instance Pretty v => Pretty (Corn v) where
    prettyPrec prec (Corn u n v0 v1) = 
        prettyPrecApp prec "Corn" [anyPretty u,anyPretty n,anyPretty v0,anyPretty v1] 


corn :: (Ord v) => CornerPosition' -> CornerCount -> v -> v -> Corn v
corn pos n u0 u1 =
                if u0 <= u1
                   then Corn pos           n u0 u1
                   else Corn (n - pos - 1) n u1 u0

normalSurfaceToPreRenderable
  :: forall s v i. (i ~ Integer, Integral i, Show v, Ord v, Pretty i, ShortShow v, StandardCoords s i) =>
     SPQWithCoords v
     -> Admissible s
     -> PreRenderable (SC2 (Corn v))
normalSurfaceToPreRenderable (SPQWithCoords spq coords _) ns =  
    let

        tris :: [Triple (Corn v)]
        tris = do
            cx <- concreteTris ns 
            let (viewI -> I i x) = c_type cx

            let f corner = 
                    mkCorn 
                        (unPos $ posOfCornerOfTri cx corner)
                        (i ./ corner)

            [ map3 f (normalCorners x) ]

        quads :: [Quadruple (Corn v)]
        quads = do
            cx <- concreteQuads ns
            let (viewI -> I i x) = c_type cx
            let f corner = 
                    mkCorn 
                        (unPos $ posOfCornerOfQuad cx corner)
                        (i ./ corner)

            [ map4 f (normalCorners x) ]


        (sc,quadDiagonals) = fromTrisAndQuads tris quads

        mkCorn :: CornerPosition' -> INormalCorner -> Corn v
        mkCorn pos nonCanonicalINormalCorner =
            let
                n = numberOfCornersOfType ns nonCanonicalINormalCorner
                (u0,u1) = map2 (spq_map spq)
                               (vertices (iNormalCornerGetContainingEdge nonCanonicalINormalCorner))
            in
                corn pos (fromIntegral n) u0 u1

        cornerCoords (Corn pos n v0 v1) = 
                interpolate (fi (1+pos) / fi (1+n)) (coords v0) (coords v1)


        isVisible = 
                            foldAnySimplex2
                                (const Visible)
                                (\e -> if Set.member e quadDiagonals
                                         then Invisible
                                         else Visible)
                                (const Visible)

        
    in

            pr_setVisibility isVisible

                (mkPreRenderable
                    cornerCoords
                    sc)



instance (Ord v, GluingMappable v) => GluingMappable (Corn v) where
    gluingMap glu (Corn i n v1 v2) = (corn i n `on` gluingMap glu) v1 v2


isRegardedAsSimplexByDisjointUnionDeriving ''DIM0 (conT ''Corn `appT` varT (mkName "v"))

instance Lift v => Lift (Corn v) where 
    lift (Corn a b c d) =
        [| corn a b c d |]

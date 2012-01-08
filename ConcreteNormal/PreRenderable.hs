{-# LANGUAGE TemplateHaskell, FlexibleContexts, CPP, RecordWildCards, NoMonomorphismRestriction, FlexibleInstances, StandaloneDeriving, GADTs, ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

module ConcreteNormal.PreRenderable(
    module SimplicialPartialQuotient,
    module PreRenderable,
    module Simplicial.SimplicialComplex,
    Corn,corn,
    normalSurfaceToPreRenderable,
    CornerPosition,CornerCount,

    ) where

import Data.Set as Set
import Data.Vect.Double(interpolate)
import HomogenousTuples
import StandardCoordinates
import PreRenderable
import PrettyUtil
import Simplicial.AnySimplex
import Simplicial.SimplicialComplex
import SimplicialPartialQuotient
import TriangulationCxtObject
import ShortShow
import ConcreteNormal
import Data.Function

type CornerCount = Int

-- | An interior point of an edge of the 'SimplicialPartialQuotient'
data Corn v = Corn CornerPosition CornerCount v v
                    -- INVARIANT: Third field <= fourth field
    deriving(Eq,Ord,Show)

instance ShortShow v => ShortShow (Corn v) where
    shortShow (Corn cp _ v0 v1) = shortShow (v0,v1) ++ show cp


instance Pretty v => Pretty (Corn v) where
    prettyPrec prec (Corn u n v0 v1) = 
        prettyPrecApp prec "Corn" [anyPretty u,anyPretty n,anyPretty v0,anyPretty v1] 


corn :: (Ord v) => CornerPosition -> CornerCount -> v -> v -> Corn v
corn pos n u0 u1 =
                if u0 <= u1
                   then Corn pos           n u0 u1
                   else Corn (n - pos - 1) n u1 u0

normalSurfaceToPreRenderable
  :: forall s v i. (Integral i, Ord v, Pretty i, ShortShow v, NormalSurface s i) =>
     SPQWithCoords v
     -> s
     -> PreRenderable (OTuple (Corn v))
normalSurfaceToPreRenderable (SPQWithCoords spq coords _) ns =  
    let
        tr = spq_tr spq

        tris :: [Triple (Corn v)]
        tris = do
            cx <- concreteTris tr ns 
            let (viewI -> I i x) = c_type cx

            let f corner = 
                    mkCorn 
                        (posOfCornerOfTri ns cx corner)
                        (i ./ corner)

            [ map3 f (normalCorners x) ]

        quads :: [Quadruple (Corn v)]
        quads = do
            cx <- concreteQuads tr ns
            let (viewI -> I i x) = c_type cx
            let f corner = 
                    mkCorn 
                        (posOfCornerOfQuad ns cx corner)
                        (i ./ corner)

            [ map4 f (normalCorners x) ]


        (sc,quadDiagonals) = fromTrisAndQuads tris quads

        mkCorn :: CornerPosition -> INormalCorner -> Corn v
        mkCorn pos nonCanonicalINormalCorner =
            let
                n = numberOfCornersOfType ns nonCanonicalINormalCorner
                (u0,u1) = map2 (spq_map spq)
                               (vertices (iNormalCornerGetContainingEdge nonCanonicalINormalCorner))
            in
                corn pos (fromIntegral n) u0 u1

        cornerCoords (Corn pos n v0 v1) = 
                interpolate (fi (1+pos) / fi (1+n)) (coords v0) (coords v1)

        
    in

                (mkPreRenderable
                    (cornerCoords . unOT)
                    sc)

                    { pr_visible = \asi -> elimAnySimplexWithNat asi (\n ->
                            caseNat2 n
                                (const True)
                                (\e -> not (Set.member e quadDiagonals))
                                (const (const True))) 
                    }


instance (Ord v, GluingMappable v) => GluingMappable (Corn v) where
    gluingMap glu (Corn i n v1 v2) = (corn i n `on` gluingMap glu) v1 v2

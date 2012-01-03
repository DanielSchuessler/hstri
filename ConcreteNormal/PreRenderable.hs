{-# LANGUAGE TemplateHaskell, FlexibleContexts, CPP, RecordWildCards, NoMonomorphismRestriction, FlexibleInstances, StandaloneDeriving, GADTs, ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

module ConcreteNormal.PreRenderable(
    module SimplicialPartialQuotient,
    module PreRenderable,
    module Simplicial.SimplicialComplex,
    Corn,
    standardCoordinatesToPreRenderable

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


-- | An interior point of an edge of the 'SimplicialPartialQuotient'
data Corn v = Corn CornerPosition Int v v
    deriving(Eq,Ord,Show)

instance ShortShow v => ShortShow (Corn v) where
    shortShow (Corn _ _ v0 v1) = shortShow (v0,v1)


instance Pretty v => Pretty (Corn v) where
    prettyPrec prec (Corn u n v0 v1) = 
        prettyPrecApp prec "Corn" [anyPretty u,anyPretty n,anyPretty v0,anyPretty v1] 


standardCoordinatesToPreRenderable
  :: forall s v i. (Integral i, Ord v, Pretty i, ShortShow v, NormalSurface s i) =>
     SPQWithCoords v
     -> s
     -> PreRenderable (OTuple (Corn v))
standardCoordinatesToPreRenderable (SPQWithCoords spq coords _) ns =  
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
                if u0 <= u1
                   then Corn (fi pos)         (fi n) u0 u1
                   else Corn (fi n - pos - 1) (fi n) u1 u0

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



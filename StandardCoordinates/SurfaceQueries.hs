{-# LANGUAGE FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module StandardCoordinates.SurfaceQueries where

import StandardCoordinates.MatchingEquations
import ConcreteNormal
import Control.Applicative
import TriangulationCxtObject
import HomogenousTuples
import MathUtil
import Data.Ratio
import Control.Monad
import Control.Exception
import Data.Numbering
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.PatriciaTree
import GraphUtil

eulerC_core
  :: (Fractional a, Integral i, StandardCoords q i) =>
     Admissible q -> a
eulerC_core adm = sum (f <$> discAssocs adm)
    where
        tr = adm_Triangulation adm

        f (d,n) = fromIntegral n*eitherIND ft fq d
        ft t = sum3 (map3 recipDeg (normalCorners t)) - 1/2
               - triArcCorrection t
        fq q = sum4 (map4 recipDeg (normalCorners q)) - 1
               - quadArcCorrection q
        
        recipDeg = recip . fromIntegral . degreeOfEdge . pMap tr . iNormalCornerGetContainingEdge

        triArcCorrection =
            if isClosedTriangulation tr
               then const 0
               else ( sum3 . map3 oneHalfIfBoundaryNormalArc . normalArcs)

        quadArcCorrection = 
            if isClosedTriangulation tr
               then const 0
               else ( sum4 . map4 oneHalfIfBoundaryNormalArc . normalArcs )

        oneHalfIfBoundaryNormalArc na =
            if isBoundaryNormalArc . pMap tr $ na
               then 0.5
               else 0


eulerC
  :: forall q i. (Show i, Integral i, Show q, StandardCoords q i) =>
     Admissible q -> i
eulerC s = case ratioToIntegral (eulerC_core s :: Ratio i) of
                Just i -> i
                Nothing -> error ("eulerC_core returned "++show (eulerC_core s :: Ratio i)++
                                    " on input "++show s)




is2Sphere :: (Show r, Integral r, Show q, StandardCoords q r) => Admissible q -> Bool
is2Sphere s = eulerC s == 2 && isClosedSurface s

is2SphereOrDisk
  :: (Show q, StandardCoords q Integer) => Admissible q -> Bool
is2SphereOrDisk = liftM2 (||) is2Sphere isDisk

-- surfaceBoundaryComponents tr s =
--     mkEquivalence0 (do
--         d <- normalDiscs s

isDisk
  :: (Show q, StandardCoords q Integer) => Admissible q -> Bool
isDisk s = case components . snd . surfaceBoundary $ s of
                   [_] -> eulerC s == 1
                   _ -> False

isClosedSurface :: StandardCoords q r => Admissible q -> Bool
isClosedSurface s = 
    all (isInnerNormalArc . pMap (adm_Triangulation s) . fst) (arcAssocs s)


surfaceBoundary
  :: StandardCoords s Integer =>
     Admissible s
     -> (Numbering (TConcrete INormalCorner),
         Gr (TConcrete INormalCorner) (TConcrete INormalArc))
surfaceBoundary s = 
    let
        c = toConcrete s

        cornerNu = nuFromList (c_corners c) 

        gr = mkGraphWithNu cornerNu
                (c_corners c)

                [   (c0,c1,a) | 
                
                        a <- c_arcs c
                    ,   let (c0,c1) = vertices a ]
    in

        (cornerNu,idPatGr gr)
            



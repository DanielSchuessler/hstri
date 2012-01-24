{-# LANGUAGE ViewPatterns, TupleSections, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module Triangulation.VertexLink where

import StandardCoordinates.Class
import QuadCoordinates.Class
import TriangulationCxtObject
import Triangulation.Class
import Test.QuickCheck
import Control.Applicative

newtype VertexLink = VertexLink TVertex 
    deriving Show

-- | Constructs the vertex link around the given vertex.
vertexLinkingSurface :: TVertex -> VertexLink
vertexLinkingSurface = VertexLink

-- | Calculates the euler characteristic of the vertex link.
vertexLinkEulerC :: TVertex -> Rational
vertexLinkEulerC v = eulerCRatio (getTriangulation v) . vertexLinkingSurface $ v 

instance QuadCoords VertexLink Integer where
    quadCount = const (const 0)
    quadAssocs = const []

instance StandardCoords VertexLink Integer where
    triCount (VertexLink v) v' = 
        if v == pMap (getTriangulation v) (iNormalTriGetVertex v')
           then 1
           else 0


    triAssocs (VertexLink v) = 
        map ((,1) . iNormalTri) $ preimageListOfVertex v

isManifoldTriangulation :: ToTriangulation t => t -> Bool
isManifoldTriangulation (toTriangulation -> tr) = 
    all (is2SphereOrDisk tr . vertexLinkingSurface) (vertices tr) 


-- | For QuickCheck.
newtype ManifoldTriangulation t = ManifoldTriangulation t
    deriving (Show,ToTriangulation)

-- | Generates only manifold triangulations.
instance (Arbitrary t, ToTriangulation t) => Arbitrary (ManifoldTriangulation t) where
    arbitrary = ManifoldTriangulation <$> (arbitrary `suchThat` isManifoldTriangulation) 

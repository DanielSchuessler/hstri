{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, ViewPatterns, TupleSections, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module Triangulation.VertexLink where

import StandardCoordinates.Class
import QuadCoordinates.Class
import TriangulationCxtObject
import Triangulation.Class
import Math.SparseVector
import Control.Arrow
import FileLocation
import StandardCoordinates.MatchingEquations
import StandardCoordinates.SurfaceQueries

newtype VertexLink = VertexLink TVertex 
    deriving (Show,Eq,Ord)

-- | Constructs the vertex link around the given vertex.
vertexLinkingSurface :: TVertex -> Admissible VertexLink
vertexLinkingSurface v_ = 
    $(fromRht) . standard_admissible (getTriangulation v_) . VertexLink $ v_

-- | Calculates the euler characteristic of the vertex link.
vertexLinkEulerC :: TVertex -> Integer
vertexLinkEulerC v = eulerC . vertexLinkingSurface $ v 

instance NormalSurfaceCoefficients VertexLink Integer

instance QuadCoords VertexLink Integer where
    quadCount = const (const 0)
    quadAssocs = const []
    quadAsSparse = const sparse_zero

instance StandardCoords VertexLink Integer where
    discCount = default_discCount_from_triQuadCount
    discAssocs = discAssocsDistinct
    discAssocsDistinct = map (first iNormalTriToINormalDisc) . triAssocsDistinct 
    standardAsSparse = sparse_fromAssocs . discAssocsDistinct 

    triCount (VertexLink v) v' = 
        if v == pMap (getTriangulation v) (iNormalTriGetVertex v')
           then 1
           else 0

    triAssocs = triAssocsDistinct
    triAssocsDistinct (VertexLink v) = 
        map (,1) . vertexLinkingSurfaceTris $ v



isManifoldTriangulation :: ToTriangulation t => t -> Bool
isManifoldTriangulation (toTriangulation -> tr) = 
    if isClosedTriangulation tr
       then isClosedManifoldTriangulation_knownClosed tr
       else all (is2SphereOrDisk . vertexLinkingSurface) (vertices tr) 

isClosedManifoldTriangulation :: ToTriangulation t => t -> Bool
isClosedManifoldTriangulation (toTriangulation -> tr) = 
    isClosedTriangulation tr && isClosedManifoldTriangulation_knownClosed tr

isClosedManifoldTriangulation_knownClosed :: Triangulation -> Bool
isClosedManifoldTriangulation_knownClosed tr =
    all ((==2) . eulerC . vertexLinkingSurface) (vertices tr) 



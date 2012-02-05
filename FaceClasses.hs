{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module FaceClasses where

import Element
import HomogenousTuples
import Data.Tuple.Index

-- | Things that have a collection of vertices
class AsList (Verts a) => Vertices a where
    type Verts a
    vertices :: a -> Verts a

vertexList :: (Vertices a) => a -> [Vert a]
vertexList = asList . vertices

type Vert a = Element (Verts a)

-- | Things that have a collection of edges
class AsList (Eds a) => Edges a where
    type Eds a
    edges :: a -> Eds a

edgeList :: Edges a => a -> [Ed a]
edgeList = asList . edges

type Ed a = Element (Eds a)
type Arc a = Ed a

class AsList (Tris a) => Triangles a where
    type Tris a
    triangles :: a -> Tris a

triangleList :: Triangles a => a -> [Tri a]
triangleList = asList . triangles

type Tri a = Element (Tris a)

class AsList (OEds a) => OEdges a where
    type OEds a
    oedges :: a -> OEds a

type OEd a = Element (OEds a)

class Link a b c | a b -> c where
    -- | The faces of the 'star' which are disjoint from the first arg.
    link :: a -> b -> c

class Star a b c | a b -> c where
    -- | The faces of the second arg containing the first arg.
    star :: a -> b -> c

class Dual a b | a -> b, b -> a where
    dual :: a -> b

class AsList (Tets a) => Tetrahedra a where
    type Tets a

    tetrahedra :: a -> Tets a

type Tet a = Element (Tets a) 

tetrahedronList :: Tetrahedra a => a -> [Tet a]
tetrahedronList = asList . tetrahedra

class (Vertices e, Verts e ~ Pair (Vert e)) => EdgeLike e
class (Edges t, Eds t ~ Triple (Ed t)) => TriangleLike t
class (Triangles tet, Tris tet ~ Quadruple (Tet tet)) => TetrahedronLike tet

instance (Vertices e, Verts e ~ Pair (Vert e)) => EdgeLike e
instance (Edges t, Eds t ~ Triple (Ed t)) => TriangleLike t
instance (Triangles tet, Tris tet ~ Quadruple (Tet tet)) => TetrahedronLike tet

verticesOfE :: EdgeLike e => e -> Pair (Vert e)
verticesOfE = vertices

edgesOfT :: TriangleLike t => t -> Triple (Ed t)
edgesOfT = edges

trisOfTet :: TetrahedronLike tet => tet -> Quadruple (Tri tet) 
trisOfTet = triangles 

edgeGetVertexAt
  :: (Vertices a, Verts a ~ (t, t)) => a -> Index2 -> t
edgeGetVertexAt = tupleToFun2 . vertices

edgeGetIndexOf
  :: (Eq a1, Vertices a, Verts a ~ (a1, a1)) =>
     a -> a1 -> Maybe Index2
edgeGetIndexOf = indexOf2 . vertices

triangleGetVertexAt
  :: (Vertices t, Verts t ~ Triple v) => t -> Index3 -> v
triangleGetVertexAt = tupleToFun3 . vertices 

triangleGetIndexOf
  :: (Eq v, Vertices t, Verts t ~ Triple v) => t -> v -> Maybe Index3
triangleGetIndexOf = indexOf3 . vertices

triangleGetEdgeAt
  :: (Edges t, Eds t ~ (e, e, e)) => t -> Index3 -> e
triangleGetEdgeAt = tupleToFun3 . edges

tetrahedronGetTriangleAt
  :: (Triangles tet, Tris tet ~ (t, t, t, t)) => tet -> Index4 -> t
tetrahedronGetTriangleAt = tupleToFun4 . triangles

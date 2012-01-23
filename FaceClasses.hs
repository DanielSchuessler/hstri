{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module FaceClasses where

import Element


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



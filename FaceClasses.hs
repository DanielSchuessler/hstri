{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module FaceClasses where

import Element

-- | Things that have a collection of vertices
class AsList vertexTuple => Vertices a vertexTuple | a -> vertexTuple where
    vertices :: a -> vertexTuple

vertexList :: Vertices a vertexTuple => a -> [Element vertexTuple]
vertexList = asList . vertices

-- | Things that have a collection of edges
class AsList edgeTuple => Edges a edgeTuple | a -> edgeTuple where
    edges :: a -> edgeTuple

edgeList ::  Edges a b => a -> [Element b]
edgeList = asList . edges

class AsList triangleTuple => Triangles a triangleTuple | a -> triangleTuple where
    triangles :: a -> triangleTuple

triangleList ::  (Triangles a b) => a -> [Element b]
triangleList = asList . triangles

class AsList oedgeTuple => OEdges a oedgeTuple | a -> oedgeTuple where
    oedges :: a -> oedgeTuple

class Link a b c | a b -> c where
    -- | The faces of the 'star' which are disjoint from the first arg.
    link :: a -> b -> c

class Star a b c | a b -> c where
    -- | The faces of the second arg containing the first arg.
    star :: a -> b -> c



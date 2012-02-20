{-# LANGUAGE TupleSections, TemplateHaskell, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module FaceClasses where

import Element
import HomogenousTuples
import Data.Tuple.Index
import PrettyUtil
import qualified Data.Vector.Generic as VG
import Language.Haskell.TH
import THUtil
import Util

data DIMMINUS1
data DIM0
data DIM1
data DIM2
data DIM3

type family Succ dim
type instance Succ DIMMINUS1 = DIM0
type instance Succ DIM0 = DIM1
type instance Succ DIM1 = DIM2
type instance Succ DIM2 = DIM3

dimTName :: Int -> Name
dimTName (-1) = ''DIMMINUS1
dimTName i = mkName ("DIM"++show i)


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
    -- | If the edges have some sort of natural order, this function should return the edges in the corresponding lexicographical order.
    edges :: a -> Eds a

edgeList :: Edges a => a -> [Ed a]
edgeList = asList . edges

edgeVector :: (VG.Vector v (Ed a), Edges a) => a -> v (Ed a)
edgeVector = VG.fromList . edgeList

type Ed a = Element (Eds a)
type Arc a = Ed a

-- | Things that have a collection of triangles
class AsList (Tris a) => Triangles a where
    type Tris a

    -- | If the vertices have some sort of natural order, this function should return the triangles in the corresponding lexicographical order (e.g. ABC, ABD, ACD, BCD for a tetrahedron with vertices A,B,C,D).
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
class (Edges t, Eds t ~ Triple (Ed t), Vertices t, Verts t ~ Triple (Vert t), Vert t ~ Vert (Ed t), EdgeLike (Ed t)) => TriangleLike t
class (Triangles tet, Tris tet ~ Quadruple (Tri tet), Edges tet, Eds tet ~ Sextuple (Ed tet), Ed tet ~ Ed (Tri tet), Vertices tet, Verts tet ~ Quadruple (Vert tet), Vert tet ~ Vert (Tri tet)) => TetrahedronLike tet

instance (Vertices e, Verts e ~ Pair (Vert e)) => EdgeLike e
instance (Edges t, Eds t ~ Triple (Ed t), Vertices t, Verts t ~ Triple (Vert t), Vert t ~ Vert (Ed t), EdgeLike (Ed t)) => TriangleLike t
instance (Triangles tet, Tris tet ~ Quadruple (Tri tet), Edges tet, Eds tet ~ Sextuple (Ed tet), Ed tet ~ Ed (Tri tet), Vertices tet, Verts tet ~ Quadruple (Vert tet), Vert tet ~ Vert (Tri tet)) => TetrahedronLike tet

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

tetrahedronGetEdgeAt
  :: (Edges tet, Eds tet ~ (e, e, e, e, e, e)) => tet -> Index6 -> e
tetrahedronGetEdgeAt = tupleToFun6 . edges

tetrahedronGetVertexAt
  :: (Vertices tet, Verts tet ~ (v, v, v, v)) => tet -> Index4 -> v
tetrahedronGetVertexAt = tupleToFun4 . vertices


data EdgeInTriangle t = EdgeInTriangle {
    eInT_triangle :: t,
    eInT_ix :: Index3
}
    deriving (Show,Eq,Ord)

instance Show t => Pretty (EdgeInTriangle t) where
    prettyPrec = prettyPrecFromShow


eInT_edge :: (Edges t, Eds t ~ (e, e, e)) => EdgeInTriangle t -> e
eInT_edge (EdgeInTriangle t i) = triangleGetEdgeAt t i

eInT_otherEdges
  :: (Edges t, Eds t ~ (e, e, e)) => EdgeInTriangle t -> Pair e
eInT_otherEdges (EdgeInTriangle t i) = map2 (triangleGetEdgeAt t) (i3_others i)

data VertexInTriangle t = VertexInTriangle {
    vInT_triangle :: t,
    vInT_ix :: Index3
}

vInT_vertex
  :: (Vertices t, Verts t ~ (v, v, v)) => VertexInTriangle t -> v
vInT_vertex (VertexInTriangle t i) = triangleGetVertexAt t i

instance Vertices (EdgeInTriangle t) where
    type Verts (EdgeInTriangle t) = Pair (VertexInTriangle t)

    vertices (EdgeInTriangle t i) = 
        map2 (VertexInTriangle t) (tupleToFun3 (subtuples3_2 allIndex3') i)


-- | Note that the returned vertex /may/ lie on the given edge; it is merely the only vertex of the triangle that isn't /forced/ to lie on the given edge
eInT_dual :: EdgeInTriangle t -> VertexInTriangle t
eInT_dual (EdgeInTriangle t i) = VertexInTriangle t (i3reverse i) 

-- | Note that the returned vertex /may/ lie on the given edge; it is merely the only vertex of the triangle that isn't /forced/ to lie on the given edge
eInT_dualVertex
  :: (TriangleLike t, v ~ Vert t) => EdgeInTriangle t -> v
eInT_dualVertex = vInT_vertex . eInT_dual

vInT_dual :: VertexInTriangle t -> EdgeInTriangle t
vInT_dual (VertexInTriangle t i) = EdgeInTriangle t (i3reverse i) 

instance Dual (VertexInTriangle t) (EdgeInTriangle t) where dual = vInT_dual
instance Dual (EdgeInTriangle t) (VertexInTriangle t) where dual = eInT_dual


inheritVertices :: (Convertible accessor ExpQ,Convertible sub TypeQ,Convertible super TypeQ) =>sub -> super -> accessor -> Q [Dec]
inheritVertices = inheritSingleArgClass ''Vertices ['vertices] [''Verts]

inheritEdges :: (Convertible accessor ExpQ,Convertible sub TypeQ,Convertible super TypeQ) =>sub -> super -> accessor -> Q [Dec]
inheritEdges = inheritSingleArgClass ''Edges ['edges] [''Eds]

inheritTriangles :: (Convertible accessor ExpQ,Convertible sub TypeQ,Convertible super TypeQ) =>sub -> super -> accessor -> Q [Dec]
inheritTriangles = inheritSingleArgClass ''Triangles ['triangles] [''Tris]

inheritTetrahedra :: (Convertible accessor ExpQ,Convertible sub TypeQ,Convertible super TypeQ) =>sub -> super -> accessor -> Q [Dec]
inheritTetrahedra = inheritSingleArgClass ''Tetrahedra ['tetrahedra] [''Tets]

inheritToDim2 :: (Convertible accessor ExpQ,Convertible sub TypeQ,Convertible super TypeQ) =>sub -> super -> accessor -> Q [Dec]
inheritToDim2 sub super ac = 
    concatMapM (\f -> f sub super ac) [inheritVertices,inheritEdges,inheritTriangles]

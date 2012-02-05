{-# LANGUAGE TypeFamilies, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
-- | The tetrahedron
module AbsTet where

import TIndex
import Tetrahedron.Vertex
import HomogenousTuples
import Tetrahedron.Edge
import Tetrahedron.Triangle
import Data.Tuple.OneTuple
import Simplicial.DeltaSet3

instance HasTIndex TIndex AbsTet where
    viewI = flip I AbsTet 
    (./) = const

data AbsTet = AbsTet
    deriving(Eq,Ord,Show)

absTet = AbsTet

instance Vertices AbsTet where
    type Verts AbsTet = Quadruple Vertex 
    vertices = const allVertices'

instance Edges AbsTet where
    type Eds AbsTet = Sextuple Edge 
    edges = const allEdges'

instance Triangles AbsTet where
    type Tris AbsTet = Quadruple Triangle 
    triangles = const allTriangles'

instance SatisfiesSimplicialIdentities3 AbsTet

instance Tetrahedra AbsTet where
    type Tets AbsTet = OneTuple AbsTet
    tetrahedra = OneTuple

instance Link Vertex (ZeroSkeleton AbsTet) (Triple Vertex) where
    link v _ = fromList3 (filter4 (/= v) allVertices')

instance Link IVertex (ZeroSkeleton AbsTet) (Triple IVertex) where
    link v p = traverseI map3 (flip link p) v

-- | Edges containing a given vertex
instance Star Vertex (OneSkeleton AbsTet) (Triple Edge) where
    star v _ = fromList3 (filter6 (isVertexOfEdge v) allEdges')

-- | Edges containing a given vertex
instance Star IVertex (OneSkeleton AbsTet) (Triple IEdge) where
    star v p = traverseI map3 (flip star p) v

-- | Triangles containing a given edge
instance Star Edge (TwoSkeleton AbsTet) (Pair Triangle) where
    star = const . trianglesContainingEdge 

instance Star Vertex (TwoSkeleton AbsTet) (Triple Triangle) where
    star = const . trianglesContainingVertex 

-- | Triangles containing a given vertex
instance Star IVertex (TwoSkeleton AbsTet) (Triple ITriangle) where
    star v p = traverseI map3 (flip star p) v

-- | Triangles containing a given edge
instance Star IEdge (TwoSkeleton AbsTet) (Pair ITriangle) where
    star e p = traverseI map2 (flip star p) e

-- = 'triangleDualVertex'
instance Link Triangle AbsTet Vertex where 
    link t _ = triangleDualVertex t

-- = 'itriangleDualVertex'
instance Link ITriangle AbsTet IVertex where 
    link t _ = iTriangleDualVertex t

-- = 'triangleByDualVertex'
instance Link Vertex AbsTet Triangle where 
    link v _ = triangleByDualVertex v

-- = 'itriangleByDualVertex'
instance Link IVertex AbsTet ITriangle where 
    link v _ = iTriangleByDualVertex v


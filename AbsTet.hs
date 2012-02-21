{-# LANGUAGE ViewPatterns, TemplateHaskell, TypeFamilies, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
-- | The tetrahedron
module AbsTet where

import TIndex
import Tetrahedron.Vertex
import HomogenousTuples
import Tetrahedron.Edge
import Tetrahedron.Triangle
import Data.Tuple.OneTuple
import Simplicial.DeltaSet3
import TupleTH
import Control.Exception
import EitherC

instance HasTIndex TIndex AbsTet where
    viewI = flip I AbsTet 
    (./) = const

data AbsTet = AbsTet
    deriving(Eq,Ord,Show)

absTet :: AbsTet
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
    star = const . edgesContainingVertex

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




-- | Morphism that embeds the abstract tetrahedron into an arbitrary tetrahedron-like thing
data MapAbsTet tet = MapAbsTet tet

instance (t ~ Tri tet, SatisfiesSimplicialIdentities3 tet) => 
    DeltaSetMorphism2 Triangle t (MapAbsTet tet) where

    mapVert (MapAbsTet tet) = tetrahedronGetVertexAt tet . toEnum . fromEnum 

    mapEd (MapAbsTet tet) = tetrahedronGetEdgeAt tet . toEnum . fromEnum 

    mapTri (MapAbsTet tet) = tetrahedronGetTriangleAt tet . toEnum . fromEnum  

    
instance (SatisfiesSimplicialIdentities3 tet) =>
    DeltaSetMorphism3 AbsTet tet (MapAbsTet tet) where

    mapTet (MapAbsTet tet) _  = tet


instance SimplicialTet AbsTet where
    sTetAscTotal = const . return $ AbsTet
    sTetVerts = $unEitherC "AbsTet/sTetVerts" . asc4total . vertices -- could skip check

instance SimplicialTet TIndex where
    sTetAscTotal (unAsc4 -> xs) = 
        let x = $(proj 4 1) xs
        in 
            assert (all3 ((== (getTIndex x)) . getTIndex) ($(dropTuple 4 1) xs)) $
            return (getTIndex x)

    sTetVerts = $unEitherC "TIndex/sTetVerts" . asc4total . vertices -- could skip check


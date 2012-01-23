{-# LANGUAGE ViewPatterns, TypeFamilies, TypeSynonymInstances #-}
module Tetrahedron.Pieces where

import Tetrahedron.Triangle
import Tetrahedron.NormalDisc
import TIndex
import Element

newtype VertexSmallTet = VertexSmallTet { vstVertex :: Vertex } 

instance Vertices VertexSmallTet where
    type Verts VertexSmallTet = [Either Vertex NormalCorner] 

    vertices (VertexSmallTet v) =
        Left v : map (Right . normalCorner) (asList (star v (OneSkeleton AbsTet))) 

newtype Stump = Stump { stumpMissingVertex :: Vertex } 

instance Vertices Stump where
    type Verts Stump = [Either Vertex NormalCorner] 

    vertices (Stump v) =

        let t = triangleByDualVertex $ v
        in
            map Left (vertexList t) ++
            map (Right . normalCorner) (asList (star v (OneSkeleton AbsTet)))


cutAlongNormalTri :: NormalTri -> (VertexSmallTet, Stump)
cutAlongNormalTri (normalTriGetVertex -> v) = (VertexSmallTet v, Stump v)

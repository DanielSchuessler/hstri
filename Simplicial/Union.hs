{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -F -pgmF cpphs -optF --cpp #-}
module Simplicial.Union where

import Simplicial.DeltaSet3

data Union s t = Union s t
    deriving(Eq)

foldUnion k (Union s t) = k s t

bimapUnion fs ft (Union s t) = Union (fs s) (ft t) 

#define F(Vertices,Vert,vertices,vertexList)\
instance (Vertices s, Vertices t, Vert s ~ Vert t) => Vertices (Union s t) where {\
    type Vert##s (Union s t) = [Vert s];\
    vertices = foldUnion (++) . bimapUnion vertexList vertexList}


F(Vertices,Vert,vertices,vertexList)
F(Edges,Ed,edges,edgeList)
F(Triangles,Tri,triangles,triangleList)
F(Tetrahedra,Tet,tetrahedra,tetrahedronList)

#undef F

instance (Vertices s, Vertices t, Vert s ~ Vert t) => Vertices (Union s t) where {\

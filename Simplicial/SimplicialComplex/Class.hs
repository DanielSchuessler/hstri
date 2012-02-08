{-# LANGUAGE TemplateHaskell, FlexibleInstances, ViewPatterns, FlexibleContexts, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module Simplicial.SimplicialComplex.Class where

import Simplicial.DeltaSet3
import Data.AscTuples
import DisjointUnion
import Data.List(partition)
import EitherC

-- | Edges that have two distinct vertices, and are uniquely determined by them.
class (Vertices e, Ord (Vert e), EdgeLike e) => SimplicialEdge e where {
    sEdgeAscTotal :: Asc2 (Vert e) -> EitherC LErrorCall e;
    sEdgeVerts :: e -> Asc2 (Vert e);
    };

instance Ord v => SimplicialEdge (Asc2 v) where {
    sEdgeAscTotal = return;
    sEdgeVerts = id };

-- | Tris that have three distinct vertices, and are uniquely determined by them.
class (Vertices t, Ord (Vert t), TriangleLike t, SatisfiesSimplicialIdentities2 t) => SimplicialTriangle t where {
    sTriangleAscTotal :: Asc3 (Vert t) -> EitherC LErrorCall t;
    sTriangleVerts :: t -> Asc3 (Vert t);
    };

instance Ord v => SimplicialTriangle (Asc3 v) where {
    sTriangleAscTotal = return;
    sTriangleVerts = id };

-- | Tetrahedra that have three distinct vertices, and are uniquely determined by them.
class (Vertices tet, Ord (Vert tet), TetrahedronLike tet, SatisfiesSimplicialIdentities3 tet) => SimplicialTet tet where {
    sTetAscTotal :: Asc4 (Vert tet) -> EitherC LErrorCall tet;
    sTetVerts :: tet -> Asc4 (Vert tet);
    };

instance Ord v => SimplicialTet (Asc4 v) where {
    sTetAscTotal = return;
    sTetVerts = id };







class (DeltaSet1 s, SimplicialEdge (Ed s)
        , Vert s ~ Vert (Ed s))  => SimplicialComplex1 s where {};

class (DeltaSet2 s, SimplicialTriangle (Tri s)
        , Vert s ~ Vert (Tri s), SimplicialComplex1 s)  => SimplicialComplex2 s where {};

class (DeltaSet3 s, SimplicialTet (Tet s)
        , Vert s ~ Vert (Tet s), SimplicialComplex2 s)  => SimplicialComplex3 s where {};



sEdgeAsc :: (SimplicialEdge e) => Asc2 (Vert e) -> e  
sEdgeAsc = $unEitherC . sEdgeAscTotal

sTriangleAsc :: (SimplicialTriangle t) => Asc3 (Vert t) -> t  
sTriangleAsc = $unEitherC . sTriangleAscTotal   

sTetAsc :: (SimplicialTet tet) => Asc4 (Vert tet) -> tet  
sTetAsc = $unEitherC . sTetAscTotal   

sEdge :: (SimplicialEdge c) => (Vert c, Vert c) -> c
sEdge = sEdgeAsc . asc2
sTriangle :: (SimplicialTriangle c) => (Vert c, Vert c, Vert c) -> c
sTriangle = sTriangleAsc . asc3
sTet :: ( SimplicialTet c) => (Vert c, Vert c, Vert c, Vert c) -> c
sTet = sTetAsc . asc4


-- sEdgeVertsAsc :: (Ord v, Vertices e, Verts e ~ (v, v)) => e -> Asc2 v
-- sEdgeVertsAsc = asc2 . vertices 
-- 
-- sTriangleVertsAsc :: (Ord v, Vertices a, Verts a ~ (v, v, v)) => a -> Asc3 v
-- sTriangleVertsAsc = asc3 . vertices
-- 
-- sTetVertsAsc :: (Ord v, Vertices a, Verts a ~ (v, v, v, v)) => a -> Asc4 v
-- sTetVertsAsc = asc4 . vertices



simplicialFaces10' :: SimplicialEdge a => a -> Pair (Vert a)
simplicialFaces10' = vertices . sEdgeVerts

simplicialFaces21'
  :: (Show (Vert t),
      SimplicialTriangle t,
      SimplicialEdge e,
      Vert e ~ Vert t) =>
     t -> Triple e
simplicialFaces21' = map3 sEdgeAsc . edges . sTriangleVerts

simplicialFaces32'
  :: (Show (Vert tet),
      SimplicialTet tet,
      SimplicialTriangle t,
      Vert tet ~ Vert t)
     =>
     tet -> Quadruple t
simplicialFaces32' = map4 sTriangleAsc . triangles . sTetVerts


djErrMsg :: [Char]
djErrMsg = "vertices are not from the same disjoint union summand"

instance (SimplicialEdge a, SimplicialEdge b) => SimplicialEdge (DJSimp DIM1 a b) where
    sEdgeAscTotal (unAsc2 -> (u,v)) =
        either'
            (\ua -> 
              either'
                (\va -> (fmap left' . sEdgeAscTotal . asc2) (ua,va))
                (const ($failureStr djErrMsg)) 
                v)

            (\ub -> 
              either'
                (const ($failureStr djErrMsg))
                (\vb -> (fmap right' . sEdgeAscTotal . asc2) (ub,vb))
                v)
            u

    sEdgeVerts = (asc . map2 left' . unAsc . sEdgeVerts) |||| 
                 (asc . map2 right' . unAsc . sEdgeVerts)

instance (SimplicialTriangle a, SimplicialTriangle b) => SimplicialTriangle (DJSimp DIM2 a b) where
    sTriangleAscTotal (unAsc3 -> (u,v,w)) =
        either'
            (\ua -> 
              either'
                (\va -> 
                    either'
                        (\wa -> (fmap left' . sTriangleAscTotal . asc3) (ua,va,wa))
                        (const ($failureStr djErrMsg   ))
                        w)
                (const ($failureStr djErrMsg   )) 
                v)

            (\ub -> 
              either'
                (const ($failureStr djErrMsg   ))
                (\vb ->
                    either'
                        (const ($failureStr djErrMsg   ))
                        (\wb -> (fmap right' . sTriangleAscTotal . asc3) (ub,vb,wb))
                        w)
                v)
            u

    sTriangleVerts = 
        (asc . map3 left' . unAsc . sTriangleVerts) |||| 
        (asc . map3 right' . unAsc . sTriangleVerts)


instance (SimplicialTet a, SimplicialTet b) => SimplicialTet (DJSimp DIM3 a b) where
    sTetAscTotal (asList -> vs) =
        case partition isLeft vs of
             (_,[]) -> (fmap left' . sTetAscTotal . asc4 . fromList4 . map fromLeft') vs
             ([],_) -> (fmap right' . sTetAscTotal . asc4 . fromList4 . map fromRight') vs
             _ -> $failureStr djErrMsg

    sTetVerts = 
        (asc . map4 left' . unAsc . sTetVerts) |||| 
        (asc . map4 right' . unAsc . sTetVerts)



mapSimpEdTotal :: (SimplicialEdge x, SimplicialEdge x') =>
     (Vert x -> Vert x') -> x -> EitherC LErrorCall x'
mapSimpEdTotal f x = sEdgeAscTotal =<< mapAscTotal f (sEdgeVerts x)

mapSimpTriTotal :: (SimplicialTriangle x, SimplicialTriangle x') =>
     (Vert x -> Vert x') -> x -> EitherC LErrorCall x'
mapSimpTriTotal f x = sTriangleAscTotal =<< mapAscTotal f (sTriangleVerts x)

mapSimpTetTotal :: (SimplicialTet x, SimplicialTet x') =>
     (Vert x -> Vert x') -> x -> EitherC LErrorCall x'
mapSimpTetTotal f x = sTetAscTotal =<< mapAscTotal f (sTetVerts x)

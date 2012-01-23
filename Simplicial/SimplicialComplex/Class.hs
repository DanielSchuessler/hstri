{-# LANGUAGE ViewPatterns, FlexibleContexts, TypeFamilies, NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -pgmPcpphs -optP--cpp #-}
module Simplicial.SimplicialComplex.Class where

import Simplicial.DeltaSet3
import Data.AscTuples
import Control.Exception
import Data.Maybe
import HomogenousTuples
import DisjointUnion
import Data.List(partition)

#define F(Edge,Pair,i,e)\
class (Vertices e, Ord (Vert e)) => Simplicial##Edge e where {\
    s##Edge##AscMay :: Asc##i (Vert e) -> Maybe e;\
    s##Edge##Verts :: e -> Asc##i (Vert e);\
    };\
instance Ord v => Simplicial##Edge (Asc##i v) where {\
    s##Edge##AscMay = Just;\
    s##Edge##Verts = id };

F(Edge,Pair,2,e)
F(Triangle,Triple,3,t)
F(Tet,Quadruple,4,tet)
#undef F

#define F(i,Edge,Ed,PREV)\
class (DeltaSet##i s, Simplicial##Edge (Ed s)\
        , Vert s ~ Vert (Ed s), PREV)  => SimplicialComplex##i s where {};\

F(1,Edge,Ed,()~())
F(2,Triangle,Tri,SimplicialComplex1 s)
F(3,Tet,Tet,SimplicialComplex2 s)
#undef F

sEdgeAsc = fromMaybe (assert False undefined) . sEdgeAscMay
sTriangleAsc = fromMaybe (assert False undefined) . sTriangleAscMay   
sTetAsc = fromMaybe (assert False undefined) . sTetAscMay   

sEdge = sEdgeAsc . asc2
sTriangle = sTriangleAsc . asc3
sTet = sTetAsc . asc4


-- sEdgeVertsAsc :: (Ord v, Vertices e, Verts e ~ (v, v)) => e -> Asc2 v
-- sEdgeVertsAsc = asc2 . vertices 
-- 
-- sTriangleVertsAsc :: (Ord v, Vertices a, Verts a ~ (v, v, v)) => a -> Asc3 v
-- sTriangleVertsAsc = asc3 . vertices
-- 
-- sTetVertsAsc :: (Ord v, Vertices a, Verts a ~ (v, v, v, v)) => a -> Asc4 v
-- sTetVertsAsc = asc4 . vertices


simplicialFaces10 :: Asc2 v -> Pair v
simplicialFaces10 x = map2 (flip deleteAt2 $ unAsc2 x) ((0,1) :: Pair Int)

simplicialFaces21 :: (Ord v, Show v) => Asc3 v -> Triple (Asc2 v)
simplicialFaces21 x = map3 (unsafeAsc . (flip deleteAt3 $ unAsc3 x)) ((0,1,2) :: Triple Int)

simplicialFaces32
  :: (Ord v, Show v) => Asc4 v -> Quadruple (Asc3 v)
simplicialFaces32 x = map4 (unsafeAsc . (flip deleteAt4 $ unAsc4 x)) ((0,1,2,3) :: Quadruple Int)

simplicialFaces10' :: SimplicialEdge a => a -> Pair (Vert a)
simplicialFaces10' = simplicialFaces10 . sEdgeVerts
simplicialFaces21'
  :: (Show (Vert t),
      SimplicialTriangle t,
      SimplicialEdge e,
      Vert e ~ Vert t) =>
     t -> Triple e
simplicialFaces21' = map3 sEdgeAsc . simplicialFaces21 . sTriangleVerts
simplicialFaces32'
  :: (Show (Vert tet),
      SimplicialTet tet,
      SimplicialTriangle t,
      Vert tet ~ Vert t)
     =>
     tet -> Quadruple t
simplicialFaces32' = map4 sTriangleAsc . simplicialFaces32 . sTetVerts

instance (SimplicialEdge a, SimplicialEdge b) => SimplicialEdge (DJSimp a b) where
    sEdgeAscMay (unAsc2 -> (u,v)) =
        either'
            (\ua -> 
              either'
                (\va -> (fmap left' . sEdgeAscMay . asc2) (ua,va))
                (const Nothing) 
                v)

            (\ub -> 
              either'
                (const Nothing)
                (\vb -> (fmap right' . sEdgeAscMay . asc2) (ub,vb))
                v)
            u

    sEdgeVerts = (asc . map2 left' . unAsc . sEdgeVerts) |||| 
                 (asc . map2 right' . unAsc . sEdgeVerts)

instance (SimplicialTriangle a, SimplicialTriangle b) => SimplicialTriangle (DJSimp a b) where
    sTriangleAscMay (unAsc3 -> (u,v,w)) =
        either'
            (\ua -> 
              either'
                (\va -> 
                    either'
                        (\wa -> (fmap left' . sTriangleAscMay . asc3) (ua,va,wa))
                        (const Nothing)
                        w)
                (const Nothing) 
                v)

            (\ub -> 
              either'
                (const Nothing)
                (\vb ->
                    either'
                        (const Nothing)
                        (\wb -> (fmap right' . sTriangleAscMay . asc3) (ub,vb,wb))
                        w)
                v)
            u

    sTriangleVerts = 
        (asc . map3 left' . unAsc . sTriangleVerts) |||| 
        (asc . map3 right' . unAsc . sTriangleVerts)


instance (SimplicialTet a, SimplicialTet b) => SimplicialTet (DJSimp a b) where
    sTetAscMay (asList -> vs) =
        case partition isLeft vs of
             (_,[]) -> (fmap left' . sTetAscMay . asc4 . fromList4 . map fromLeft') vs
             ([],_) -> (fmap right' . sTetAscMay . asc4 . fromList4 . map fromRight') vs
             _ -> Nothing

    sTetVerts = 
        (asc . map4 left' . unAsc . sTetVerts) |||| 
        (asc . map4 right' . unAsc . sTetVerts)



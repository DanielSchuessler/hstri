{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module Edge where

import Control.Exception
import Data.BitSet.Word8 as BitSet
import Data.List as List
import Data.Maybe
import Element
import HomogenousTuples
import Test.QuickCheck
import Text.PrettyPrint.ANSI.Leijen
import Util
import Vertex

-- | Edge of an abstract tetrahedron (unoriented)
newtype Edge = Edge (BitSet Vertex) 
    deriving(Eq,Ord) 


instance Enum Edge where
    toEnum n = allEdges !! n
    fromEnum e@(Edge x) = fromMaybe (error ("Invalid edge: "++show x)) (findIndex (==e) allEdges)


prop_EnumEdge :: Property
prop_EnumEdge = forAll (elements [0..5]) (\n -> fromEnum (toEnum n :: Edge) .=. n)

instance Show Edge where
    show (edgeVertices -> (v0,v1)) = show v0 ++ show v1

instance Bounded Edge where
    minBound = eAB
    maxBound = eCD

eAB,eAC,eAD,eBC,eBD,eCD :: Edge
eAB = edge (vA,vB) 
eAC = edge (vA,vC) 
eAD = edge (vA,vD) 
eBC = edge (vB,vC) 
eBD = edge (vB,vD) 
eCD = edge (vC,vD) 

class MakeEdge a where
    edge :: a -> Edge

allEdges ::  [Edge]
allEdges = asList allEdges'

allEdges' ::  (Edge, Edge, Edge, Edge, Edge, Edge)
allEdges' = ( eAB , eAC , eAD , eBC , eBD , eCD  ) 

isVertexOfEdge :: Vertex -> Edge -> Bool
isVertexOfEdge v (Edge x) = BitSet.member v x

edgeVertices :: Edge -> (Vertex, Vertex)
edgeVertices e = fromList2 ( filter4 (`isVertexOfEdge` e) allVertices' ) 

-- | The argument vertices must be distinct.
instance MakeEdge (Vertex,Vertex) where
    edge (v0,v1) = assert (v0 /= v1) $ Edge (BitSet.insert v0 $ BitSet.singleton v1)


-- | The BitSet must have exactly two elements
instance MakeEdge (BitSet Vertex) where
    edge b = assert (BitSet.size b == 2) (Edge b)

-- | Construct a vertex as the intersection of two edges
instance MakeVertex (Pair Edge) where
    vertex es = case uncurry intersectEdges es of
                     Just (Right v) -> v
                     _ -> error ("vertex "++show es)


intersectEdges :: Edge -> Edge -> Maybe (Either Edge Vertex)
intersectEdges e1 e2 = case filter2 (\v -> elem2 v (edgeVertices e2)) (edgeVertices e1) of
                            [] -> Nothing
                            y -> Just (case y of
                                            [v] -> assert (e1/=e2) (Right v)
                                            [_,_] -> assert (e1==e2) (Left e1)
                                            _ -> assert False (error "impossible")
                                      )
               

edgePrettyColor :: Doc -> Doc
edgePrettyColor = cyan

instance Pretty Edge where pretty = edgePrettyColor . text . show

oppositeEdge ::  Edge -> Edge
oppositeEdge (Edge e) = Edge (BitSet.complement e)


prop_OppositeEdge_Order2 ::  Edge -> Property
prop_OppositeEdge_Order2 e = let e' = oppositeEdge e in (e' /= e) .&. (oppositeEdge e' == e)



prop_OppositeEdge_disjoint ::  Edge -> Property
prop_OppositeEdge_disjoint e = List.intersect (edgeVertexList e) (edgeVertexList (oppositeEdge e)) .=. [] 
    where
        edgeVertexList = asList . edgeVertices

instance Arbitrary Edge where arbitrary = elements allEdges
instance Finite Edge

instance Quote Edge where
    quotePrec _ e = "e" ++ show e


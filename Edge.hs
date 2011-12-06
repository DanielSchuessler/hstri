{-# LANGUAGE FlexibleContexts, BangPatterns, TypeFamilies, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Edge (
    module Vertex,

    -- * Plain
    Edge, eAB,eAC,eAD,eBC,eBD,eCD,
    allEdges,allEdges',
    MakeEdge(..),
    oppositeEdge,
    edgeToBitSet,
    bitSetToEdge,
    isVertexOfEdge,
    intersectEdges,
    otherVertex,

    -- * Oriented
    OEdge,
    MakeOEdge(..),
    verticesToOEdge,


    -- * Indexed
    IEdge,

    -- * Oriented and indexed
    OIEdge,
    allOIEdges,

    -- * Testing
    qc_Edge
    )
    where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.BitSet.Word8 as BitSet
import Data.List as List
import Data.Maybe
import Data.Word
import Element
import HomogenousTuples
import OrderableFace
import QuickCheckUtil
import Quote
import S2
import THUtil() -- Lift BitSet
import Test.QuickCheck
import Test.QuickCheck.All
import Text.PrettyPrint.ANSI.Leijen hiding((<$>))
import TupleTH
import Util
import Vertex
import Language.Haskell.TH.Syntax

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

-- | An 'Edge' with a tetrahedron index attached to it
data IEdge = IEdge {-# UNPACK #-} !TIndex {-# UNPACK #-} !Edge
    deriving (Eq,Ord)

instance Enum IEdge where
    toEnum (toEnum -> I i x) = (./) i x
    fromEnum = fromEnum . viewI

instance HasTIndex IEdge Edge where
    viewI (IEdge i x) = I i x
    (./) = IEdge

instance Show IEdge where show = show . viewI
instance Quote IEdge where quotePrec prec = quotePrec prec . viewI
instance Pretty IEdge where pretty = pretty . viewI
                            
                            
-- | Oriented edge of an abstract tetrahedron
newtype OEdge = OEdge Word8 {- The lower nibble encodes the first vertex, the upper nibble encodes the second vertex. Invariant: The vertices are distinct. -}
    deriving(Eq,Ord)

instance Enum OEdge where 
    fromEnum (unpackOrderedFace -> (g,e)) = fromEnum (EnumPair g e)
    toEnum n = packOrderedFace g (e::Edge) where EnumPair g e = toEnum n


prop_EnumOEdge :: Property
prop_EnumOEdge = forAll (elements [0..11]) (\n -> fromEnum (toEnum n :: OEdge) .=. n)


-- | Make an oriented edge of an abstract tetrahedron from two of its vertices (which must be distinct)
verticesToOEdge ::  (Vertex, Vertex) -> OEdge
verticesToOEdge (map2 vertexToWord8 -> (v0, v1)) = OEdge (nibblesToWord8 (v0,v1))

instance Bounded OEdge where
    minBound = verticesToOEdge (vA,vB)
    maxBound = verticesToOEdge (vD,vC)

class MakeOEdge a where
    oedge :: a -> OEdge


instance MakeOEdge (Pair Vertex) where
    oedge = verticesToOEdge


instance (OrderableFace IEdge OIEdge) where
    type VertexSymGroup IEdge = S2
    type VertexTuple IEdge = Pair IVertex
    unpackOrderedFace (viewI -> I i (unpackOrderedFace -> (g,e))) = (g, (./) i e)
    packOrderedFace g = mapI (packOrderedFace g)

instance LeftAction S2 OIEdge where (.*) = defaultLeftActionForOrderedFace

-- | Vertices contained in a given edge
instance Vertices Edge (Pair Vertex) where
    vertices = edgeVertices


instance OrderableFace Edge OEdge where 
    type VertexSymGroup Edge = S2
    type VertexTuple Edge = Pair Vertex

    unpackOrderedFace (vertices -> (v0,v1)) =
      let
        e = edge (v0,v1)
      in
        case compare v0 v1 of
             LT -> (NoFlip, e)
             GT -> (Flip  , e)
             EQ -> error ("Invalid OEdge with vertices "++show (v0,v1))

    packOrderedFace g e = verticesToOEdge (g .* vertices e) 

instance LeftAction S2 OEdge where
    (.*) = defaultLeftActionForOrderedFace

instance Show OEdge where
    show (vertices -> (v0,v1)) = show v0 ++ show v1

instance Vertices OEdge (Pair Vertex) where
    vertices (OEdge (word8ToNibbles -> nibbles)) = map2 vertexFromWord8 nibbles

instance Arbitrary OEdge where arbitrary = liftM2 packOrderedFace arbitrary arbitrary 
instance Pretty OEdge where pretty = abstractTetrahedronColor . text . show


-- | An 'OEdge' with a tetrahedron index attached to it
type OIEdge = I OEdge 

instance Finite OEdge

instance Vertices IEdge (Pair IVertex) where 
    {-# INLINABLE vertices #-}
    vertices z = map2 ((./) (getTIndex z)) (vertices (forgetTIndex z)) 

instance Vertices OIEdge (Pair IVertex) where 
    vertices z = map2 ((./) (getTIndex z)) (vertices (forgetTIndex z)) 


-- | Edges containing a given vertex
instance Edges Vertex (Triple Edge) where
    edges !v = fromList3 (filter6 (isVertexOfEdge v) allEdges')


trivialHasTIndexInstance [t|OEdge|]

instance Edges IVertex (Triple IEdge) where
    edges (viewI -> I i v) = map3 (i ./) (edges v) 

instance Arbitrary IEdge where
    arbitrary = (./) <$> arbitrary <*> arbitrary


allOIEdges :: TIndex -> (Dodecatuple OIEdge)
allOIEdges ti = $(catTuples 6 6) (map6 (packOrderedFace NoFlip) es) (map6 (packOrderedFace Flip) es)
    where
        es = edges ti


otherVertex :: (Eq a1, Vertices a (a1, a1)) => a -> a1 -> a1
otherVertex (vertices -> (v0,v1)) v =
    if v0==v
    then v1
    else if v1==v
    then v0
    else error ("otherVertex: Second arg not a vertex of first arg")

instance Link Vertex Edge Vertex where
    link = flip otherVertex

instance Link Vertex OEdge Vertex where
    link = flip otherVertex

instance Link IVertex IEdge IVertex where
    link = flip otherVertex

instance Link IVertex OIEdge IVertex where
    link = flip otherVertex

qc_Edge :: IO Bool
qc_Edge = $(quickCheckAll)

edgeToBitSet :: Edge -> BitSet Vertex
edgeToBitSet (Edge bs) = bs

bitSetToEdge :: BitSet Vertex -> Edge
bitSetToEdge = Edge

instance Edges TIndex (Sextuple IEdge) where
    edges z = map6 (z ./) allEdges'

instance Lift Edge where
    lift (Edge e) = [| Edge e |]

{-# LANGUAGE DeriveGeneric, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, BangPatterns, TypeFamilies, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Tetrahedron.Edge (
    module Tetrahedron.Vertex,

    -- * Plain
    Edge, 
    -- ** Constants
    eAB,eAC,eAD,eBC,eBD,eCD,
    allEdges,allEdges',
    -- ** Properties
    edgeToBitSet,
    isVertexOfEdge,
    intersectEdges,
    otherVertex,
    edgeVertices,
    -- ** Construction
    MakeEdge(..),
    edgeByVertices,
    oppositeEdge,
    bitSetToEdge,


    -- * Oriented
    OEdge,
    MakeOEdge(..),
    verticesToOEdge,


    -- * Indexed
    IEdge,
    iEdgeByVertices,
    oppositeIEdge,

    -- * Oriented and indexed
    OIEdge,
    allOIEdges,
    oiEdgeByVertices,

    -- * Misc
    edgeNu,oEdgeNu,

    )
    where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Binary
import Data.Binary.Derive
import Data.BitSet.Word8 as BitSet
import Data.Function
import Data.List as List
import Data.Maybe
import Element
import GHC.Generics(Generic)
import HomogenousTuples
import Language.Haskell.TH.Syntax
import OrderableFace
import PrettyUtil
import Quote
import Math.Groups.S2
import ShortShow
import THUtil() -- Lift BitSet
import Test.QuickCheck
import TupleTH
import Util
import Tetrahedron.Vertex
import Data.Numbering


deriving instance Binary (BitSet Vertex)

-- | Edge of an abstract tetrahedron (unoriented)
newtype Edge = Edge (BitSet Vertex) 
    deriving(Eq,Ord,Binary) 


instance Enum Edge where
    toEnum n = allEdges !! n
    fromEnum e@(Edge x) = fromMaybe (error ("Invalid edge: "++show x)) (findIndex (==e) allEdges)



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

allEdges' ::  (Sextuple Edge)
allEdges' = ( eAB , eAC , eAD , eBC , eBD , eCD  ) 

isVertexOfEdge :: Vertex -> Edge -> Bool
isVertexOfEdge v (Edge x) = BitSet.member v x

edgeVertices :: Edge -> (Pair Vertex)
edgeVertices e = fromList2 ( filter4 (`isVertexOfEdge` e) allVertices' ) 

edgeByVertices :: (Pair Vertex) -> Edge
edgeByVertices (v0,v1) = assert (v0 /= v1) $ Edge (BitSet.insert v0 $ BitSet.singleton v1)

-- | The argument vertices must be distinct.
instance MakeEdge (Pair Vertex) where
    edge = edgeByVertices


-- | The BitSet must have exactly two elements
instance MakeEdge (BitSet Vertex) where
    edge b = assert (BitSet.size b == 2) (Edge b)

-- | Construct a vertex as the intersection of two edges
instance MakeVertex (Pair Edge) where
    vertex es = case uncurry intersectEdges es of
                     Just (Right v) -> v
                     _ -> error ("vertex "++show es)


intersectEdges :: Edge -> Edge -> Maybe (Either Edge Vertex)
intersectEdges e1 e2 = case (intersect2_2 `on` edgeVertices) e1 e2 of
                            [] -> Nothing
                            y -> Just (case y of
                                            [v] -> assert (e1/=e2) (Right v)
                                            [_,_] -> assert (e1==e2) (Left e1)
                                            _ -> assert False (error "impossible")
                                      )
               

edgePrettyColor :: Doc -> Doc
edgePrettyColor = cyan

instance Pretty Edge where pretty = edgePrettyColor . text . show

oppositeEdge :: Edge -> Edge
oppositeEdge (Edge e) = Edge (BitSet.complement e)

oppositeIEdge :: IEdge -> IEdge
oppositeIEdge = mapI oppositeEdge


instance Arbitrary Edge where arbitrary = elements allEdges
instance Finite Edge

instance Quote Edge where
    quotePrec _ e = "e" ++ show e

-- | An 'Edge' with a tetrahedron index attached to it
data IEdge = IEdge {-# UNPACK #-} !TIndex {-# UNPACK #-} !Edge
    deriving (Eq,Ord,Generic)

instance Binary IEdge where
    put = derivePut
    get = deriveGet

instance Enum IEdge where
    toEnum (toEnum -> I i x) = (./) i x
    fromEnum = fromEnum . viewI

instance HasTIndex IEdge Edge where
    viewI (IEdge i x) = I i x
    (./) = IEdge

instance Show IEdge where show = show . viewI
instance Quote IEdge where quotePrec prec = quotePrec prec . viewI
instance Pretty IEdge where pretty = pretty . viewI
instance ShortShow IEdge where shortShow = shortShow . viewI
                            
                            
-- | Oriented edge of an abstract tetrahedron
newtype OEdge = OEdge Word8 {- The lower nibble encodes the first vertex, the upper nibble encodes the second vertex. Invariant: The vertices are distinct. -}
    deriving(Eq,Ord,Binary)

instance Enum OEdge where 
    fromEnum (unpackOrderedFace -> (e,g)) = fromEnum (EnumPair e g)
    toEnum n = packOrderedFace e g where EnumPair e g = toEnum n




-- | Make an oriented edge of an abstract tetrahedron from two of its vertices (which must be distinct)
verticesToOEdge ::  (Pair Vertex) -> OEdge
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
    unpackOrderedFace = defaultUnpackOrderedFaceI
    packOrderedFace = defaultPackOrderedFaceI


instance RightAction S2 OIEdge where (*.) = defaultRightActionForOrderedFace

-- | Vertices contained in a given edge
instance Vertices Edge where

    type Verts Edge = Pair Vertex
    vertices = edgeVertices


instance OrderableFace Edge OEdge where 
    type VertexSymGroup Edge = S2

    unpackOrderedFace (vertices -> (v0,v1)) =
      let
        e = edge (v0,v1)
      in
        case compare v0 v1 of
             LT -> (e,NoFlip)
             GT -> (e,Flip)
             EQ -> error ("Invalid OEdge with vertices "++show (v0,v1))

    packOrderedFace e g = verticesToOEdge (vertices e *. g) 


instance RightAction S2 OEdge where
    (*.) = defaultRightActionForOrderedFace

instance Show OEdge where
    show (vertices -> (v0,v1)) = show v0 ++ show v1

instance Vertices OEdge where
    type Verts OEdge = Pair Vertex

    vertices (OEdge (word8ToNibbles -> nibbles)) = map2 vertexFromWord8 nibbles

instance Arbitrary OEdge where arbitrary = liftM2 packOrderedFace arbitrary arbitrary 
instance Pretty OEdge where pretty = abstractTetrahedronColor . text . show


-- | An 'OEdge' with a tetrahedron index attached to it
type OIEdge = I OEdge 

instance Finite OEdge

instance Vertices IEdge where 
    type Verts IEdge = Pair IVertex

    {-# INLINABLE vertices #-}
    vertices = traverseI map2 vertices

instance Vertices OIEdge where 
    type Verts OIEdge = Pair IVertex

    vertices = traverseI map2 vertices




trivialHasTIndexInstance [t|OEdge|]


instance Arbitrary IEdge where
    arbitrary = (./) <$> arbitrary <*> arbitrary


allOIEdges :: TIndex -> (Dodecatuple OIEdge)
allOIEdges ti = $(catTuples 6 6) (map6 (flip packOrderedFace NoFlip) es) 
                                 (map6 (flip packOrderedFace Flip) es)
    where
        es = edges ti


otherVertex :: (Eq a1, Vertices a, Verts a ~ (a1, a1)) => a -> a1 -> a1
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


edgeToBitSet :: Edge -> BitSet Vertex
edgeToBitSet (Edge bs) = bs

bitSetToEdge :: BitSet Vertex -> Edge
bitSetToEdge = Edge

instance Edges TIndex where
    type Eds TIndex = Sextuple IEdge

    edges z = map6 (z ./) allEdges'

instance Lift Edge where
    lift (Edge e) = [| Edge e |]

instance ShortShow Edge where shortShow = show
instance ShortShow OEdge where shortShow = show


iEdgeByVertices
  :: Pair IVertex -> IEdge
iEdgeByVertices = withTIndexEqual edgeByVertices

oiEdgeByVertices
  :: Pair IVertex -> OIEdge
oiEdgeByVertices = withTIndexEqual verticesToOEdge

edgeNu :: Numbering Edge
edgeNu = finiteTypeNu

oEdgeNu :: Numbering OEdge
oEdgeNu = finiteTypeNu

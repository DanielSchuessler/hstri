{-# LANGUAGE DeriveDataTypeable, FunctionalDependencies, DeriveGeneric, StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, BangPatterns, TypeFamilies, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Tetrahedron.Edge (
    module Data.Tuple.Index,
    module Tetrahedron.Vertex,
    module Simplicial.SimplicialComplex.Class,
    module Data.AscTuples,

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
    oppositeEdge0,
    bitSetToEdge,
    edgesContainingVertex,
    edgesContainingVertexAsc,


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
    edgeNu,oEdgeNu,TetrahedronEdge(..)

    )
    where

import Control.Applicative
import Control.DeepSeq
import Control.DeepSeq.TH
import Control.Exception
import Control.Monad
import Data.Binary
import Data.Binary.Derive
import Data.Bits
import Data.Foldable(foldMap)
import Data.Function
import Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Numbering
import Data.Tuple.Index
import Element
import GHC.Generics(Generic)
import HomogenousTuples
import Language.Haskell.TH.Lift
import Math.Groups.S2
import OrderableFace
import OrphanInstances()
import PrettyUtil
import Quote
import ShortShow
import Simplicial.SimplicialComplex.Class
import THUtil() -- Lift BitSet
import Test.QuickCheck(Arbitrary(..),elements)
import Tetrahedron.Vertex
import TupleTH
import Util
import Data.AscTuples
import Data.Typeable


-- | Edge of an abstract tetrahedron (unoriented)
newtype Edge = Edge Word8
                    -- Only the least significant 4 bits are used.
                    -- The i-th least significant bit signifies whether the i-th vertex is part of the edge
    deriving(Eq,Ord,Binary,NFData,Typeable) 

edgeToBitSet :: Edge -> Word8
edgeToBitSet (Edge bs) = bs

-- | The BitSet must have exactly two elements
bitSetToEdge :: Word8 -> Edge
bitSetToEdge b = 
 assert (getSum (foldMap (\i -> Sum $ if testBit b i then 1::Int else 0) [0..3]) == 2) $
                Edge b
               


-- | Must remain in @allEdges'@-compatible order!!
instance Enum Edge where
    toEnum n = allEdges !! n
    fromEnum e@(Edge x) = fromMaybe (error ("Invalid edge: "++show x)) (findIndex (==e) allEdges)

-- | Must remain in @Enum Edge@-compatible order!!
allEdges' ::  (Sextuple Edge)
allEdges' = ( eAB , eAC , eAD , eBC , eBD , eCD  ) 

-- | Must remain in @Enum Edge@-compatible order!!
allEdges ::  [Edge]
allEdges = asList allEdges'

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



isVertexOfEdge :: Vertex -> Edge -> Bool
isVertexOfEdge v (Edge x) = testBit x (fromEnum v)

edgeVertices :: Edge -> (Vertex, Vertex)
edgeVertices = unAsc2 . edgeVerticesAsc

edgeVerticesAsc :: Edge -> Asc2 Vertex
edgeVerticesAsc e = 
    --    fromList2 ( filter4 (`isVertexOfEdge` e) allVertices' ) 

    UnsafeAsc2 $ case edgeToBitSet e of
        3 -> (A,B)
        5 -> (A,C)
        9 -> (A,D)
        6 -> (B,C)
        10 -> (B,D)
        12 -> (C,D)
        _ -> assert False undefined

    -- table made with: 
    -- concatMap (\x@(Edge w) -> show (unBitSet w) ++ " -> " ++ show (edgeVertices x) ++ "\n") allEdges

edgeByVertices :: (Pair Vertex) -> Edge
edgeByVertices (v0,v1) = assert (v0 /= v1) $ Edge (shiftL 1 (fromEnum v0) .|. shiftL 1 (fromEnum v1))  

-- | The argument vertices must be distinct.
instance MakeEdge (Pair Vertex) where
    edge = edgeByVertices


-- | The BitSet must have exactly two elements
instance MakeEdge Word8 where
    edge = bitSetToEdge 

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

oppositeEdge0 :: Edge -> Edge
oppositeEdge0 (Edge e) = Edge (complement e .&. 0xF)

oppositeIEdge :: IEdge -> IEdge
oppositeIEdge = mapI oppositeEdge0


instance Arbitrary Edge where arbitrary = elements allEdges
instance Finite Edge

instance Quote Edge where
    quotePrec _ e = "e" ++ show e

instance Quote OEdge where
    quotePrec _ e = "o" ++ show e

-- | An 'Edge' with a tetrahedron index attached to it
data IEdge = IEdge {-# UNPACK #-} !TIndex {-# UNPACK #-} !Edge
    deriving (Eq,Ord,Generic,Typeable)

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
    deriving(Eq,Ord,Binary,NFData,Typeable)

deriveLift ''OEdge

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



instance Edges TIndex where
    type Eds TIndex = Sextuple IEdge

    edges z = map6 (z ./) allEdges'

instance Lift Edge where
    lift (Edge e) = [| bitSetToEdge e |]

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

instance Lift IEdge where
    lift (viewI -> I x y) = [| x ./ y |] 

class (Vertices e, Verts e ~ Pair v) => TetrahedronEdge v e | e -> v where
    oppositeEdge :: e -> e

instance TetrahedronEdge Vertex Edge where
    oppositeEdge = oppositeEdge0

instance TetrahedronEdge IVertex IEdge where
    oppositeEdge = oppositeIEdge

-- TH
mapTIndicesFromHasTIndex [t|IEdge|]
deriveNFData ''IEdge


instance SimplicialEdge Edge where
    sEdgeAscTotal = return . edgeByVertices . unAsc2 
    sEdgeVerts = edgeVerticesAsc

instance SimplicialEdge IEdge where
    sEdgeAscTotal = return . iEdgeByVertices . unAsc2 
    sEdgeVerts = unsafeAsc . vertices

instance Show a => Show (Edge -> a) where show = showFiniteFunc "e"
instance Show a => Show (OEdge -> a) where show = showFiniteFunc "e"

-- if you change this, change edgesContainingVertexAsc!
edgesContainingVertex :: Vertex -> Triple Edge
edgesContainingVertex v = 
    -- generated from: \v -> fromList3 (filter6 (isVertexOfEdge v) allEdges')
    case v of
        A -> (eAB,eAC,eAD)
        B -> (eAB,eBC,eBD)
        C -> (eAC,eBC,eCD)
        D -> (eAD,eBD,eCD)

edgesContainingVertexAsc :: Vertex -> Asc3 Edge
edgesContainingVertexAsc = unsafeAsc . edgesContainingVertex

instance QuoteConstPat Edge where
    quoteConstPat (vertices -> vs) = quoteConstPatPair vs 
    quoteConstPat_view _ x = "vertices " ++ x 


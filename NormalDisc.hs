{-# LANGUAGE NoMonomorphismRestriction, ImplicitParams, TypeFamilies, TypeOperators, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances, ViewPatterns #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall #-}
module NormalDisc(
        module AbstractTetrahedron,
        module AbstractTetrahedron2,

        -- * Normal discs
        NormalDisc(..),
        MakeNormalDisc(..),   allNormalDiscs,
          

        -- * Normal triangles
        NormalTri(..),
        allNormalTris, normalTriGetVertex, 
        MakeNormalTri(..),


        -- * Normal quadrilaterals
        NormalQuad(..), allNormalQuads, allNormalQuads', normalQuadByDisjointEdge, normalQuadGetDisjointEdges,
        normalQuadByVertexAndTriangle, normalQuadGetIntersectedEdges,


        -- * Normal arcs
        NormalArc, normalArcGetTriangle, normalArcGetVertex, allNormalArcs,
            normalArcGetOppositeEdge,
        MakeNormalArc(..),
        HasNormalArcs(..),
        normalArcList,

        -- * Normal corners
        NormalCorner, allNormalCorners, normalCornerGetContainingEdge, 
        MakeNormalCorner(..),
        HasNormalCorners(..),
        normalCornerList,


        -- * Testing
        qc_NormalDisc



    ) where

import AbstractTetrahedron
import AbstractTetrahedron2
import Collections
import Control.Exception(assert)
import Data.Functor
import Data.List(sort)
import Element
import HomogenousTuples
import Prelude hiding(catch,lookup)
import Test.QuickCheck
import Test.QuickCheck.All
import Text.PrettyPrint.ANSI.Leijen hiding((<$>))
import TupleTH

newtype NormalDisc = NormalDisc { unNormalDisc :: Either NormalTri NormalQuad }
    deriving(Eq,Ord,Arbitrary)



class MakeNormalDisc a where
    normalDisc :: a -> NormalDisc
    

newtype NormalTri = NormalTri Vertex deriving(Enum,Bounded,Eq,Ord,Arbitrary)

instance Show NormalTri where
    show nt = "t"++show (normalTriGetVertex nt)
--    show nt = "{Normal triangle enclosing "++show (normalTriGetVertex nt)++"}"


allNormalTris ::  [NormalTri]
allNormalTris = NormalTri <$> allVertices

data NormalQuad = 
    -- | The quad disjoint from the edges 'AB' and 'CD'.
    Q | 
    -- | The quad disjoint from the edges 'AC' and 'BD'.
    Q' | 
    -- | The quad disjoint from the edges 'AD' and 'BC'.                  
    Q'' deriving(Enum,Bounded,Eq,Ord)

instance Show NormalQuad where
    show q = "q"++(show . fst . normalQuadGetDisjointEdges) q 

--     show q = "{Normal quad separating "++show v0++","++show v1++" from "++show v2++","++show v3++"}"
--         where
--             (vertices -> (v0,v1), vertices -> (v2,v3)) = normalQuadGetDisjointEdges q

instance MakeNormalDisc NormalTri where normalDisc = NormalDisc . Left
instance MakeNormalDisc NormalQuad where normalDisc = NormalDisc . Right
instance Arbitrary NormalQuad where  arbitrary = elements [minBound .. maxBound]

allNormalQuads ::  [NormalQuad]
allNormalQuads = asList allNormalQuads'

allNormalQuads' :: Triple NormalQuad
allNormalQuads' = (Q,Q',Q'')

allNormalDiscs ::  [NormalDisc]
allNormalDiscs = (normalDisc <$> allNormalTris) ++ (normalDisc <$> allNormalQuads)

instance Enum NormalDisc where
    toEnum x | x < 4 = NormalDisc . Left $ toEnum x   
             | otherwise = NormalDisc . Right $ toEnum (x - 4)
    fromEnum = either fromEnum ((+4) . fromEnum) . unNormalDisc

instance Bounded NormalDisc where
    minBound = NormalDisc . Left $ minBound
    maxBound = NormalDisc . Right $ maxBound


normalTriGetVertex ::  NormalTri -> Vertex
normalTriGetVertex (NormalTri x) = x

instance MakeVertex NormalTri where
    vertex = normalTriGetVertex

class MakeNormalTri a where
    normalTri :: a -> NormalTri 

-- | Construct a normal triangle by the vertex it encloses
instance MakeNormalTri Vertex where
    normalTri = NormalTri

-- | Construct a normal triangle by the vertex it encloses
instance MakeNormalDisc Vertex where
    normalDisc = normalDisc . normalTri



data NormalArc = NormalArc Triangle Vertex  -- Invariant: The 'Vertex' is contained in the 'Triangle'
    deriving (Eq,Ord)

instance Show NormalArc where
    show (NormalArc t v) = "{Normal arc in "++show t++" dual to "++show v++"}" 

instance HasNormalCorners NormalArc (Pair NormalCorner) where
    normalCorners (NormalArc t v) =  
        (   fromList2
          . fmap NormalCorner
          . filter3 (v `isSubface`)) (edges t)

instance HasNormalCorners Triangle (Triple NormalCorner) where
    normalCorners = map3 normalCorner . edges

instance HasNormalCorners OTriangle (Triple NormalCorner) where
    normalCorners = map3 normalCorner . edges

instance MakeNormalCorner OEdge where
    normalCorner = normalCorner . forgetVertexOrder


prop_NormalCornersOfNormalArc_distinct :: NormalArc -> Bool
prop_NormalCornersOfNormalArc_distinct nat = let (c1,c2) = normalCorners nat in c1 /= c2


class MakeNormalArc a where
    normalArc :: a -> NormalArc

-- | Construct a normal arc by the triangle that contains it and the vertex (of that triangle) 
-- dual to the normal arc (i.e., the vertex which the normal arc separates from the other two vertices of the triangle)
instance MakeNormalArc (Triangle,Vertex) where
    normalArc (f,v) = assert (isSubface v f) 
                        (NormalArc f v)


-- | Construct a normal arc by the two edges it intersects
instance MakeNormalArc (Edge,Edge) where
    normalArc es = normalArc (triangle es, vertex es) 

-- | Construct a normal arc by its normal corners
instance MakeNormalArc (NormalCorner,NormalCorner) where
    normalArc = normalArc . map2 normalCornerGetContainingEdge 

normalQuadGetDisjointEdges ::  NormalQuad -> (Edge, Edge)
normalQuadGetDisjointEdges q = case q of
                                    Q -> (edge (vA,vB),edge (vC,vD))
                                    Q' -> (edge (vA,vC),edge (vB,vD))
                                    Q'' -> (edge (vA,vD),edge (vB,vC))

normalQuadGetIntersectedEdges ::  NormalQuad -> [Edge]
normalQuadGetIntersectedEdges (normalQuadGetDisjointEdges -> (e1,e2)) =
    $(filterTuple 6) (\e -> e /= e1 && e /= e2) allEdges'

prop_normalQuadGetIntersectedEdges ::  NormalQuad -> Bool
prop_normalQuadGetIntersectedEdges nqt = 
    sort allEdges == sort (normalQuadGetIntersectedEdges nqt ++ asList (normalQuadGetDisjointEdges nqt))



class AsList normalArcTuple => HasNormalArcs a normalArcTuple | a -> normalArcTuple where
    normalArcs :: a -> normalArcTuple

class AsList normalCornerTuple => HasNormalCorners a normalCornerTuple | a -> normalCornerTuple where
    normalCorners :: a -> normalCornerTuple

normalArcList ::  HasNormalArcs a normalArcTuple => a -> [Element normalArcTuple]
normalArcList = asList . normalArcs

normalCornerList ::  HasNormalCorners a normalCornerTuple => a -> [Element normalCornerTuple]
normalCornerList = asList . normalCorners

instance HasNormalArcs NormalQuad (Quadruple NormalArc) where
    normalArcs q = case q of
                   Q   -> go (vC,vD,vA,vB)
                   Q'  -> go (vB,vA,vD,vC)
                   Q'' -> go (vA,vB,vC,vD)
        where
            go = $(zipTupleWith 4) (curry normalArc) (tABC,tABD,tACD,tBCD)


prop_NormalDisc_NormalArcs_correct :: NormalDisc -> Bool
prop_NormalDisc_NormalArcs_correct ndt = all (`isSubface` ndt) (normalArcs ndt) 

prop_NormalDisc_NormalArcs_complete :: NormalArc -> NormalDisc -> Property
prop_NormalDisc_NormalArcs_complete nat ndt = 
    isSubface nat ndt ==> 
        any (==nat) (normalArcs ndt) 

instance Arbitrary NormalArc where
    arbitrary = elements [ minBound .. maxBound ]

-- | Constructs a normal quad specified by one of the two edges disjoint from it
normalQuadByDisjointEdge ::  Edge -> NormalQuad
normalQuadByDisjointEdge e = 
    case $(filterTuple 3) (\q -> $(elemTuple 2) e (normalQuadGetDisjointEdges q)) allNormalQuads' of
         [q] -> q
         xs -> error (unwords ["normalQuadByDisjointEdge",show e ++":", "impossible:", "q =",show xs])

instance Show NormalDisc where show = either show show . unNormalDisc

instance Pretty NormalDisc where pretty = either pretty pretty . unNormalDisc

instance Pretty NormalQuad where pretty = green . text . show 

instance Pretty NormalTri where pretty = green . text . show


-- | Constructs a normal quad by indirectly specifying one of the two edges disjoint from it using 'edgeByOppositeVertexAndTriangle' 
normalQuadByVertexAndTriangle :: Vertex -> Triangle -> NormalQuad
normalQuadByVertexAndTriangle v f = normalQuadByDisjointEdge (edgeByOppositeVertexAndTriangle v f) 



-- | Gets the triangle containing the given normal arc
normalArcGetTriangle ::  NormalArc -> Triangle
normalArcGetTriangle (NormalArc f _) = f

-- | Gets the vertex separated by the normal arc from the other two vertices in the 'normalArcGetTriangle'
normalArcGetVertex ::  NormalArc -> Vertex
normalArcGetVertex (NormalArc _ v) = v

-- | Gets the edge which is contained in the 'normalArcGetTriangle' of the 
-- given normal arc, but disjoint from the normal arc
normalArcGetOppositeEdge ::  NormalArc -> Edge
normalArcGetOppositeEdge nat = edgeByOppositeVertexAndTriangle (normalArcGetVertex nat) (normalArcGetTriangle nat)


instance Enum NormalArc where
    fromEnum (NormalArc f v) = 3 * fromEnum f + indexInTriple v (vertices f) 
    toEnum n = assert (n >= 0) $
               assert (n < 12) $
                   let 
                        (d,m) = divMod n 3
                        f :: Triangle
                        f = toEnum d
                   in normalArc (f, vertexList f !! m)

instance Bounded NormalArc where
    minBound = normalArc (f, minimum . vertexList $ f)
        where
            f :: Triangle
            f = minBound

    maxBound = normalArc (f , maximum . vertexList $ f)
        where
            f :: Triangle
            f = maxBound



allNormalArcs :: [NormalArc]
allNormalArcs = [minBound .. maxBound]

instance Pretty NormalArc where pretty = green . text . show

newtype NormalCorner = NormalCorner Edge 
    deriving (Eq,Ord,Enum,Bounded,Arbitrary)

class MakeNormalCorner a where
    normalCorner :: a -> NormalCorner

instance Show NormalCorner where
    show (NormalCorner e) = "{Normal corner on "++show e++"}"

instance Pretty NormalCorner where pretty = green . text . show

allNormalCorners :: [NormalCorner]
allNormalCorners = [minBound .. maxBound]

normalCornerGetContainingEdge ::  NormalCorner -> Edge
normalCornerGetContainingEdge (NormalCorner e) = e

instance MakeEdge NormalCorner where
    edge = normalCornerGetContainingEdge


instance MakeNormalCorner Edge where
    normalCorner = NormalCorner

instance IsSubface NormalCorner Edge where
    isSubface nct e = normalCornerGetContainingEdge nct == e 

instance IsSubface NormalCorner Triangle where
    isSubface nct t = normalCornerGetContainingEdge nct `isSubface` t

instance HasNormalArcs NormalTri (Triple NormalArc) where
    normalArcs (normalTriGetVertex -> v) = map3 (\f -> normalArc (f, v)) (triangles v)

instance HasNormalArcs NormalDisc [NormalArc] where
    normalArcs = either normalArcList normalArcList . unNormalDisc


instance HasNormalArcs Triangle (Triple NormalArc) where
    normalArcs (normalCorners -> (x0,x1,x2)) = (normalArc (x2,x0), normalArc (x0,x1), normalArc (x1,x2))

instance HasNormalArcs OTriangle (Triple NormalArc) where
    normalArcs (normalCorners -> (x0,x1,x2)) = (normalArc (x2,x0), normalArc (x0,x1), normalArc (x1,x2))

instance IsSubface NormalArc Triangle where
    isSubface nat t = normalArcGetTriangle nat == t

instance IsSubface NormalArc NormalTri where
    isSubface nat ntt = normalTriGetVertex ntt == normalArcGetVertex nat

instance IsSubface NormalArc NormalQuad where
    isSubface nat nqt = elem4 nat (normalArcs nqt)

instance IsSubface NormalArc NormalDisc where
    isSubface nat = either (isSubface nat) (isSubface nat) . unNormalDisc


prop_Triangle_NormalArcs_correct :: Triangle -> Bool
prop_Triangle_NormalArcs_correct t = all3 (`isSubface` t) (normalArcs t) 

prop_Triangle_NormalArcs_complete :: NormalArc -> Triangle -> Property
prop_Triangle_NormalArcs_complete nat t = 
    isSubface nat t ==> 
        any3 (==nat) (normalArcs t) 



qc_NormalDisc ::  IO Bool
qc_NormalDisc = $(quickCheckAll)

deriveCollectionKeyClass ''NormalArc
deriveCollectionKeyClass ''NormalCorner
deriveCollectionKeyClass ''NormalDisc




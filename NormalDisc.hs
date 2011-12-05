{-# LANGUAGE NoMonomorphismRestriction, ImplicitParams, TypeFamilies, TypeOperators, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances, ViewPatterns #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall #-}
module NormalDisc(
        module AbstractTetrahedron,
        module AbstractTetrahedron2,
        module NormalArc,

        -- * Normal discs
        NormalDisc(..),
        MakeNormalDisc(..),   allNormalDiscs, 
        NormalDiscs(..),normalDiscList,
        normalDiscsContainingNormalCorner,
          

        -- * Normal triangles
        NormalTri(..),
        allNormalTris, allNormalTris', normalTriGetVertex, 
        MakeNormalTri(..),
        NormalTris(..),normalTriList,normalTriByNormalArc,
        ntA,ntB,ntC,ntD,


        -- * Normal quadrilaterals
        NormalQuad(..), allNormalQuads, allNormalQuads', 
        normalQuadByDisjointEdge, 
        normalQuadByVertexAndTriangle, 
        normalQuadByNormalArc,
        normalQuadsByIntersectedEdge,
        normalQuadGetIntersectedEdges,
        normalQuadGetDisjointEdges,
        otherNormalQuads,
        NormalQuads(..),normalQuadList,




        -- * Testing
        qc_NormalDisc



    ) where

import AbstractTetrahedron
import AbstractTetrahedron2
import Data.Functor
import Data.List(sort)
import Element
import HomogenousTuples
import Prelude hiding(catch,lookup)
import Test.QuickCheck
import Test.QuickCheck.All
import TupleTH
import NormalArc
import Control.Monad
import Data.Maybe
import PrettyUtil

newtype NormalDisc = NormalDisc { unNormalDisc :: Either NormalTri NormalQuad }
    deriving(Eq,Ord,Arbitrary)



class MakeNormalDisc a where
    normalDisc :: a -> NormalDisc
    

newtype NormalTri = NormalTri Vertex deriving(Enum,Bounded,Eq,Ord,Arbitrary)

ntA,ntB,ntC,ntD :: NormalTri
(ntA,ntB,ntC,ntD) = map4 normalTri allVertices' 

instance Show NormalTri where
    showsPrec = prettyShowsPrec 



allNormalTris :: [NormalTri]
allNormalTris = asList allNormalTris'

allNormalTris' :: (NormalTri, NormalTri, NormalTri, NormalTri)
allNormalTris' = map4 NormalTri allVertices'

data NormalQuad = 
    -- | The quad disjoint from the edges 'eAB' and 'eCD'.
    Q_ab | 
    -- | The quad disjoint from the edges 'eAC' and 'eBD'.
    Q_ac | 
    -- | The quad disjoint from the edges 'eAD' and 'eBC'.                  
    Q_ad 
    
    deriving(Enum,Bounded,Eq,Ord)

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
allNormalQuads' = (Q_ab,Q_ac,Q_ad)

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




normalQuadGetDisjointEdges ::  NormalQuad -> (Edge, Edge)
normalQuadGetDisjointEdges q = case q of
                                    Q_ab -> (edge (vA,vB),edge (vC,vD))
                                    Q_ac -> (edge (vA,vC),edge (vB,vD))
                                    Q_ad -> (edge (vA,vD),edge (vB,vC))

-- | Edges (circularly) adjacent in the returned tuple have nonempty intersections.
normalQuadGetIntersectedEdges ::  NormalQuad -> Quadruple Edge
normalQuadGetIntersectedEdges q =
    case q of
         Q_ab -> go (vA,vC,vB,vD)
         Q_ac -> go (vA,vB,vC,vD)
         Q_ad -> go (vA,vB,vD,vC)
        
        where
            go vs = $(zipTupleWith 4) (curry edge) (rotate4_1 vs) vs


prop_normalQuadGetIntersectedEdges ::  NormalQuad -> Bool
prop_normalQuadGetIntersectedEdges nqt = 
    sort allEdges == sort (toList4 (normalQuadGetIntersectedEdges nqt) 
        ++ asList (normalQuadGetDisjointEdges nqt))







prop_NormalDisc_NormalArcs_correct :: NormalDisc -> Bool
prop_NormalDisc_NormalArcs_correct ndt = all (`isSubface` ndt) (normalArcs ndt) 

prop_NormalDisc_NormalArcs_complete :: NormalArc -> NormalDisc -> Property
prop_NormalDisc_NormalArcs_complete nat ndt = 
    isSubface nat ndt ==> 
        any (==nat) (normalArcs ndt) 


-- | Constructs a normal quad specified by one of the two edges disjoint from it
normalQuadByDisjointEdge :: Edge -> NormalQuad
normalQuadByDisjointEdge e = r
    where
        r | e == eAB = Q_ab
          | e == eAC = Q_ac
          | e == eAD = Q_ad
          | e == eCD = Q_ab
          | e == eBD = Q_ac
          | e == eBC = Q_ad
          | otherwise = error ("normalQuadByDisjointEdge: Unexpected edge")


otherNormalQuads :: NormalQuad -> (Pair NormalQuad)
otherNormalQuads Q_ab = (Q_ac,Q_ad)
otherNormalQuads Q_ac = (Q_ab,Q_ad)
otherNormalQuads Q_ad = (Q_ab,Q_ac)

normalQuadsByIntersectedEdge :: Edge -> Pair NormalQuad
normalQuadsByIntersectedEdge = otherNormalQuads . normalQuadByDisjointEdge


--     case $(filterTuple 3) (\q -> $(elemTuple 2) e (normalQuadGetDisjointEdges q)) allNormalQuads' of
--          [q] -> q
--          xs -> error (unwords ["normalQuadByDisjointEdge",show e ++":", "impossible:", "q =",show xs])

instance Show NormalDisc where show = either show show . unNormalDisc

instance Pretty NormalDisc where pretty = either pretty pretty . unNormalDisc

instance Pretty NormalQuad where pretty = green . text . show 

instance Pretty NormalTri where pretty x = green (text "nt" <> (text . show . normalTriGetVertex) x)


-- | Constructs a normal quad by indirectly specifying one of the two edges disjoint from it using 'edgeByOppositeVertexAndTriangle' 
normalQuadByVertexAndTriangle :: Vertex -> Triangle -> NormalQuad
normalQuadByVertexAndTriangle v f = normalQuadByDisjointEdge (edgeByOppositeVertexAndTriangle v f) 


-- | Returns the unique normal quad type containing a normal arc of the given type
normalQuadByNormalArc :: NormalArc -> NormalQuad
normalQuadByNormalArc na = normalQuadByVertexAndTriangle (normalArcGetVertex na) (normalArcGetTriangle na)

prop_normalQuadByNormalArc :: NormalArc -> Bool
prop_normalQuadByNormalArc na = isSubface na (normalQuadByNormalArc na) 


instance NormalArcs NormalTri (Triple NormalArc) where
    normalArcs (normalTriGetVertex -> v) = map3 (\f -> normalArc (f, v)) (triangles v)

instance NormalArcs NormalDisc [NormalArc] where
    normalArcs = either normalArcList normalArcList . unNormalDisc




instance IsSubface NormalArc NormalTri where
    isSubface nat ntt = normalTriGetVertex ntt == normalArcGetVertex nat

instance IsSubface NormalArc NormalQuad where
    isSubface nat nqt = elem4 nat (normalArcs nqt)

instance IsSubface NormalArc NormalDisc where
    isSubface nat = either (isSubface nat) (isSubface nat) . unNormalDisc




qc_NormalDisc ::  IO Bool
qc_NormalDisc = $(quickCheckAll)

-- deriveCollectionKeyClass ''NormalArc
-- deriveCollectionKeyClass ''NormalCorner
-- deriveCollectionKeyClass ''NormalDisc


instance NormalArcs NormalQuad (Quadruple NormalArc) where
    normalArcs q = case normalCorners q of
                        cs -> map4 
                                normalArc 
                                ($(zipTuple 4) (rotate4_1 cs) cs)



class AsList tuple => NormalTris a tuple | a -> tuple where
    normalTris :: a -> tuple

normalTriList :: NormalTris a b => a -> [Element b]
normalTriList = asList . normalTris

class AsList tuple => NormalQuads a tuple | a -> tuple where
    normalQuads :: a -> tuple


normalQuadList :: NormalQuads a b => a -> [Element b]
normalQuadList = asList . normalQuads

class AsList tuple => NormalDiscs a tuple | a -> tuple where
    normalDiscs :: a -> tuple


normalDiscList ::  NormalDiscs a b => a -> [Element b]
normalDiscList = asList . normalDiscs

-- | Normal triangles meeting a given edge
instance NormalTris Edge (Pair NormalTri) where
    normalTris = map2 normalTri . vertices 

instance NormalCorners NormalTri (Triple NormalCorner) where
    normalCorners nt =
        map3 normalCorner (edges . normalTriGetVertex $ nt)

instance NormalCorners NormalQuad (Quadruple NormalCorner) where
    normalCorners =
        map4 normalCorner . normalQuadGetIntersectedEdges

-- | Returns the unique normal tri type containing a normal arc of the given type
normalTriByNormalArc :: NormalArc -> NormalTri
normalTriByNormalArc = normalTri . normalArcGetVertex

prop_normalTriByNormalArc :: NormalArc -> Bool
prop_normalTriByNormalArc na = isSubface na (normalTriByNormalArc na) 

normalTrisContainingNormalCorner :: NormalCorner -> Pair NormalTri
normalTrisContainingNormalCorner = map2 normalTri . vertices . normalCornerGetContainingEdge

normalQuadsContainingNormalCorner :: NormalCorner -> Pair NormalQuad
normalQuadsContainingNormalCorner = normalQuadsByIntersectedEdge . normalCornerGetContainingEdge 

normalDiscsContainingNormalCorner :: NormalCorner -> Quadruple NormalDisc
normalDiscsContainingNormalCorner = liftM2 ($(catTuples 2 2)) 
    (map2 normalDisc . normalTrisContainingNormalCorner) 
    (map2 normalDisc . normalQuadsContainingNormalCorner)


instance Link NormalCorner NormalTri (Pair NormalCorner) where
    link nc nt = 
             fromMaybe (error (unwords ["link",show nc,show nt]))
                (normalCorners nt `deleteTuple3` nc)


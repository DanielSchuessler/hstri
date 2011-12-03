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
          

        -- * Normal triangles
        NormalTri(..),
        allNormalTris, allNormalTris', normalTriGetVertex, 
        MakeNormalTri(..),
        NormalTris(..),normalTriList,normalTriByNormalArc,


        -- * Normal quadrilaterals
        NormalQuad(..), allNormalQuads, allNormalQuads', normalQuadByDisjointEdge, normalQuadGetDisjointEdges,
        normalQuadByVertexAndTriangle, normalQuadGetIntersectedEdges,
        normalQuadByNormalArc,
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
import Text.PrettyPrint.ANSI.Leijen hiding((<$>))
import TupleTH
import NormalArc

newtype NormalDisc = NormalDisc { unNormalDisc :: Either NormalTri NormalQuad }
    deriving(Eq,Ord,Arbitrary)



class MakeNormalDisc a where
    normalDisc :: a -> NormalDisc
    

newtype NormalTri = NormalTri Vertex deriving(Enum,Bounded,Eq,Ord,Arbitrary)

instance Show NormalTri where
    show nt = "t"++show (normalTriGetVertex nt)
--    show nt = "{Normal triangle enclosing "++show (normalTriGetVertex nt)++"}"


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

normalQuadGetIntersectedEdges ::  NormalQuad -> Quadruple Edge
normalQuadGetIntersectedEdges (normalQuadGetDisjointEdges -> (e1,e2)) =
    fromList4 $
    $(filterTuple 6) (\e -> e /= e1 && e /= e2) allEdges'

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
    normalArcs q = case q of
                   Q_ab   -> go (vC,vD,vA,vB)
                   Q_ac  -> go (vB,vA,vD,vC)
                   Q_ad -> go (vA,vB,vC,vD)
        where
            go = $(zipTupleWith 4) (curry normalArc) (tABC,tABD,tACD,tBCD)



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

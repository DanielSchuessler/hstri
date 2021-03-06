{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, GADTs, NoMonomorphismRestriction, ImplicitParams, TypeFamilies, TypeOperators, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances, ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall #-}
module Tetrahedron.NormalDisc(
        module Tetrahedron,
        module Tetrahedron.NormalArc,

        -- * Normal discs
        NormalDisc,eitherND,
        MakeNormalDisc(..),   allNormalDiscs, 
        NormalDiscs(..),normalDiscList,
        normalDiscsContainingNormalCorner,
        normalDiscsContainingNormalArc,
        adjacentNormalCorners,

        -- ** GADT sillyness
        DiscShape(..),
        IsDiscShape(..),
        getShape,
        getShape1,
        eitherND',

          

        -- * Normal triangles
        NormalTri(..),
        NormalTris(..),
        normalTriList,
        -- ** Construction
        allNormalTris, allNormalTris',
        MakeNormalTri(..),
        normalTriByNormalArc,
        adjacentNormalCornersInTri,
        -- ** Properties
        normalTriGetVertex, 
        normalTriGetNormalCorners,
        normalTriGetNormalCornersAsc,
        normalTriGetNormalArcs,


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
        adjacentNormalCornersInQuad,
        normalQuadGetNormalCornersInOrder,
        normalQuadGetNormalArcsInOrder,








    ) where

import Tetrahedron
import Control.Applicative
import Control.Arrow((&&&))
import Control.Monad
import Data.Maybe
import Data.SumType
import Element
import HomogenousTuples
import Language.Haskell.TH.Syntax
import Tetrahedron.NormalArc
import Prelude hiding(catch,lookup)
import PrettyUtil
import Quote
import Test.QuickCheck
import TupleTH
import Util
import Data.Ix
import Control.DeepSeq
import Control.DeepSeq.TH
import Language.Haskell.TH.Lift
import Data.Typeable
import ShortShow
import Data.Char


    

newtype NormalTri = NormalTri Vertex deriving(Enum,Bounded,Eq,Ord,Arbitrary,Finite,Ix,NFData,Typeable)

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
    
    deriving(Enum,Bounded,Eq,Ord,Show,Ix,Typeable)


deriveNFData ''NormalQuad

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
    fromEnum = eitherND fromEnum ((+4) . fromEnum)

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

instance Show NormalDisc where show = eitherND show show

instance Pretty NormalDisc where pretty = eitherND pretty pretty

instance Pretty NormalQuad where pretty = green . text . show

instance Pretty NormalTri where pretty = green . text . show . normalTriGetVertex

instance Quote NormalTri where quote nt = "nt" ++ show (normalTriGetVertex nt)
instance Quote NormalQuad where quote = show


-- | The normal quad type having a normal arc of type 'normalArcByTriangleAndVertex'
normalQuadByVertexAndTriangle :: Vertex -> Triangle -> NormalQuad
normalQuadByVertexAndTriangle v f = normalQuadByDisjointEdge (edgeByOppositeVertexAndTriangle v f) 


-- | Returns the unique normal quad type containing a normal arc of the given type
normalQuadByNormalArc :: NormalArc -> NormalQuad
normalQuadByNormalArc na = normalQuadByVertexAndTriangle (normalArcGetVertex na) (normalArcGetTriangle na)



instance NormalArcs NormalTri (Triple NormalArc) where
    normalArcs = normalTriGetNormalArcs

instance Edges NormalTri  where
    type Eds NormalTri = (Triple NormalArc)
    edges = normalTriGetNormalArcs

instance NormalArcs NormalDisc [NormalArc] where
    normalArcs = eitherND normalArcList normalArcList




instance IsSubface NormalArc NormalTri where
    isSubface nat ntt = normalTriGetVertex ntt == normalArcGetVertex nat

instance IsSubface NormalArc NormalQuad where
    isSubface nat nqt = elem4 nat (normalArcs nqt)

instance IsSubface NormalArc NormalDisc where
    isSubface nat = eitherND (isSubface nat) (isSubface nat)

instance IsSubface NormalCorner NormalTri where
    isSubface nc nt = isVertexOfEdge (vertex nt) (edge nc)

instance IsSubface NormalCorner NormalQuad where
    isSubface nc nq = elem4 nc (normalCorners nq)

instance IsSubface NormalCorner NormalDisc where
    isSubface nat = eitherND (isSubface nat) (isSubface nat)



-- deriveCollectionKeyClass ''NormalArc
-- deriveCollectionKeyClass ''NormalCorner
-- deriveCollectionKeyClass ''NormalDisc





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

-- | Normal triangles meeting a given edge
instance NormalTris OEdge (Pair NormalTri) where
    normalTris = map2 normalTri . vertices 

-- | = normalTriGetNormalCorners
instance NormalCorners NormalTri (Triple NormalCorner) where
    normalCorners = normalTriGetNormalCorners

-- | = normalTriGetNormalCorners
instance Vertices NormalTri  where
    type Verts NormalTri = Triple NormalCorner
    vertices = normalTriGetNormalCorners

-- | In order corresponding to 'edgesContainingVertex'.
normalTriGetNormalCorners :: NormalTri -> Triple NormalCorner
normalTriGetNormalCorners = 
    map3 normalCorner . unAsc . edgesContainingVertexAsc . normalTriGetVertex


normalTriGetNormalCornersAsc :: NormalTri -> Asc3 NormalCorner
normalTriGetNormalCornersAsc = 
    unsafeAsc . normalTriGetNormalCorners

-- Ascending
normalTriGetNormalArcs :: NormalTri -> Triple NormalArc
normalTriGetNormalArcs =
--    \(normalTriGetVertex -> v) -> sort3 (map3 (\t -> normalArc (t, v)) (star v (TwoSkeleton AbsTet)))

    -- DO NOT TOUCH THE ORDERING without adjusting the QuadHalf module
    \x -> map3 (uncurry normalArcByTriangleAndVertex) $ case normalTriGetVertex x of
        A -> ((tABC, vA), (tABD, vA), (tACD, vA))
        B -> ((tABC, vB), (tABD, vB), (tBCD, vB))
        C -> ((tABC, vC), (tACD, vC), (tBCD, vC))
        D -> ((tABD, vD), (tACD, vD), (tBCD, vD))


-- | Elements (circularly) adjacent in the returned tuple are adjacent corners
normalQuadGetNormalCornersInOrder
  :: NormalQuad
     -> (Quadruple NormalCorner)
normalQuadGetNormalCornersInOrder = 
    --map4 normalCorner . normalQuadGetIntersectedEdges

    -- DO NOT TOUCH THE ORDERING without adjusting the QuadHalf module
    \x -> map4 normalCorner $ case x of
        Q_ab -> (eAD, eAC, eBC, eBD)
        Q_ac -> (eAD, eAB, eBC, eCD)
        Q_ad -> (eAC, eAB, eBD, eCD)

dihedral4 :: (t, t, t, t) -> [(t, t, t, t)]
dihedral4 x = do
    x' <- [x,
            $(rotateTuple 4 1) x, 
            $(rotateTuple 4 2) x, 
            $(rotateTuple 4 3) x 
         ]

    [x',$(reverseTuple 4) x']

-- | = 'normalQuadGetNormalCornersInOrder'
instance NormalCorners NormalQuad (Quadruple NormalCorner) where
    normalCorners = normalQuadGetNormalCornersInOrder

instance Vertices NormalQuad where
    type Verts NormalQuad = Quadruple NormalCorner
    vertices = normalQuadGetNormalCornersInOrder


-- | @i@th component of the result spans from the @i-1@th component of 'normalQuadGetNormalCornersInOrder' to its @i@th component 
normalQuadGetNormalArcsInOrder :: NormalQuad -> Quadruple NormalArc
normalQuadGetNormalArcsInOrder q =  
    case normalQuadGetNormalCornersInOrder q of
        cs -> map4 
                normalArc
                ($(zipTuple 4) cs (rotate4_1 cs))

instance NormalArcs NormalQuad (Quadruple NormalArc) where
    normalArcs = normalQuadGetNormalArcsInOrder 


-- | Returns the unique normal tri type containing a normal arc of the given type
normalTriByNormalArc :: NormalArc -> NormalTri
normalTriByNormalArc = normalTri . normalArcGetVertex


normalTrisContainingNormalCorner :: NormalCorner -> Pair NormalTri
normalTrisContainingNormalCorner = map2 normalTri . vertices . normalCornerGetContainingEdge

normalQuadsContainingNormalCorner :: NormalCorner -> Pair NormalQuad
normalQuadsContainingNormalCorner = normalQuadsByIntersectedEdge . normalCornerGetContainingEdge 

normalDiscsContainingNormalCorner :: NormalCorner -> Quadruple NormalDisc
normalDiscsContainingNormalCorner = liftM2 ($(catTuples 2 2)) 
    (map2 normalDisc . normalTrisContainingNormalCorner) 
    (map2 normalDisc . normalQuadsContainingNormalCorner)



adjacentNormalCornersInTri
  :: (Eq a, Show a, Show a1, NormalCorners a1 (a, a, a)) =>
     a -> a1 -> (a, a)
adjacentNormalCornersInTri nc nt = 
             fromMaybe _err
                (deleteTuple3 nc (normalCorners nt))


                where
                    _err = (error (unwords ["link",show nc,show nt]
                                    ++": Normal corner not contained in normal tri"))

adjacentNormalCornersInQuad
  :: NormalCorner -> NormalQuad -> (NormalCorner, NormalCorner)
adjacentNormalCornersInQuad nc nq = 
        case normalQuadGetNormalCornersInOrder nq of
             (a,b,c,d)
                | nc == a -> (d,b)
                | nc == b -> (a,c)
                | nc == c -> (b,d)
                | nc == d -> (c,a)
                | otherwise ->

                        error (unwords ["link",show nc,show nq]
                                    ++": Normal corner not contained in normal quad")
                   

adjacentNormalCorners
  :: NormalCorner -> NormalDisc -> (NormalCorner, NormalCorner)
adjacentNormalCorners = liftA2 eitherND adjacentNormalCornersInTri adjacentNormalCornersInQuad

-- | = 'adjacentNormalCornersInTri'
instance Link NormalCorner NormalTri (Pair NormalCorner) where
    link = adjacentNormalCornersInTri 

-- | = 'adjacentNormalCornersInQuad'
instance Link NormalCorner NormalQuad (Pair NormalCorner) where
    link = adjacentNormalCornersInQuad

instance Link NormalCorner NormalDisc (Pair NormalCorner) where
    link nc = eitherND (link nc) (link nc) 





instance Lift NormalTri where
    lift (NormalTri v) = [| NormalTri v |] 


instance Lift NormalDisc where
    lift (NormalDisc x) = [| NormalDisc x |] 

instance Quote NormalDisc where
    quotePrec prec x = 
        quoteParen (prec > 10) $
            "normalDisc " ++ (eitherND (quotePrec 11) (quotePrec 11) x)

instance Finite NormalQuad
instance Finite NormalDisc

instance Show a => Show (NormalDisc -> a) where show = showFiniteFunc "nd"
instance Show a => Show (NormalQuad -> a) where show = showFiniteFunc "q"
instance Show a => Show (NormalTri -> a) where show = showFiniteFunc "t"


normalDiscsContainingNormalArc
  :: NormalArc -> (Pair NormalDisc)
normalDiscsContainingNormalArc = 
    normalDisc . normalTriByNormalArc &&& 
    normalDisc . normalQuadByNormalArc 


data DiscShape :: (* -> *) where
    Tri :: DiscShape NormalTri
    Quad :: DiscShape NormalQuad

class MakeNormalDisc a => IsDiscShape a where isDiscShapeProof :: DiscShape a 

instance IsDiscShape NormalTri where isDiscShapeProof = Tri
instance IsDiscShape NormalQuad where isDiscShapeProof = Quad

-- | Convenience function; ignores its first arg
getShape :: IsDiscShape a => a -> DiscShape a
getShape = const isDiscShapeProof

-- | Convenience function; ignores its first arg
getShape1 :: IsDiscShape a => f a -> DiscShape a
getShape1 = const isDiscShapeProof

eitherND' :: (forall a. IsDiscShape a => a -> r) -> NormalDisc -> r
eitherND' k = eitherND k k


instance Ix NormalDisc where
    range (x,y) =
        either'
            (\xt ->
                either'
                    (\yt -> map left' (range (xt, yt)))
                    (\yq -> map left'  (range (xt, maxBound)) ++ 
                            map right' (range (minBound, yq))) 
                    y)

            (\xq ->
                either' 
                    (const [])
                    (\yq -> map right' (range (xq, yq)))
                    y)
            x
            
    index (x,_) z = fromEnum z - fromEnum x 
    inRange (x,y) z = x <= z && z <= y
    rangeSize (x,y) = max 0 (fromEnum y - fromEnum x + 1)

instance NormalCorners NormalDisc [NormalCorner] where
    normalCorners = eitherND normalCornerList normalCornerList


type instance L NormalDisc = NormalTri
type instance R NormalDisc = NormalQuad

newtype NormalDisc = NormalDisc { unNormalDisc :: Either NormalTri NormalQuad }
    deriving(Eq,Ord,Arbitrary,SubSumTy,SuperSumTy,NFData,Typeable)

eitherND
  :: (NormalTri -> r) -> (NormalQuad -> r) -> NormalDisc -> r
eitherND kt kq = either kt kq . unNormalDisc

class MakeNormalDisc a where
    normalDisc :: a -> NormalDisc

deriveLift ''NormalQuad

instance ShortShow NormalQuad where
    shortShow = map toUpper . drop 2 . show

instance ShortShow NormalTri where
    shortShow = shortShow . normalTriGetVertex

instance ShortShow NormalDisc where
    shortShow = eitherND shortShow shortShow


instance QuoteConstPat NormalQuad where
    quoteConstPat = show

instance QuoteConstPat NormalTri where
    quoteConstPat = quoteConstPat . normalTriGetVertex

    quoteConstPat_view _ x = "normalTriGetVertex "++x

instance Edges NormalQuad where
    type Eds NormalQuad = Quadruple NormalArc
    edges = normalQuadGetNormalArcsInOrder

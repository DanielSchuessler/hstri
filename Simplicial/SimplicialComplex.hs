{-# LANGUAGE ViewPatterns, TypeOperators, GADTs, RecordWildCards, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables, NamedFieldPuns, TypeFamilies, DefaultSignatures, FlexibleContexts, OverlappingInstances, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, CPP #-} 
{-# OPTIONS -Wall #-}
-- | All 'Pair's, 'Triple's and 'Quadruple's are assumed to be strictly ascendingly ordered in this module.
module Simplicial.SimplicialComplex where


import AbstractTetrahedron
import Control.Exception
import Control.Monad
import Data.List
import Data.Set as Set hiding(map)
import HomogenousTuples
import Simplicial.DeltaSet3
--import Simplicial.Labels
import TupleTH
import PrettyUtil
import Data.AscTuples
import DisjointUnion
import Control.Arrow


-- type Plus4 n = S (S (S (S n)))
-- 
-- type family OTuple' v n
-- type instance (OTuple' v) N0 = v
-- type instance (OTuple' v) N1 = Pair v
-- type instance (OTuple' v) N2 = Triple v
-- type instance (OTuple' v) N3 = Quadruple v
-- type instance (OTuple' v) (Plus4 n) = List v (S (Plus4 n)) 
--                                         
-- newtype OTuple v n = OT { unOT :: OTuple' v n } 
-- 
-- 
-- 
-- #define OT_DERIVE(F,OARGS,ARGS)\
--     F OARGS = caseNat4 (undefined :: n) (F ARGS) (F ARGS) (F ARGS) (F ARGS) (const (F ARGS))
-- 
-- instance (Show v, Nat n) => Show (OTuple v n) where
--     OT_DERIVE( showsPrec prec, (OT x), x )
-- 
-- instance (ShortShow v, Nat n) => ShortShow (OTuple v n) where
--     OT_DERIVE( shortShowsPrec prec, (OT x), x )
-- 
-- instance (Pretty v, Nat n) => Pretty (OTuple v n) where
--     OT_DERIVE( prettyPrec prec, (OT x), x )
-- 
-- instance (Eq v, Nat n) => Eq (OTuple v n) where
--     OT_DERIVE( (==), (OT x) (OT y), x y )
-- 
-- instance (Ord v, Nat n) => Ord (OTuple v n) where
--     OT_DERIVE( compare, (OT x) (OT y), x y )
-- 
-- 
-- #undef OT_DERIVE
-- 
-- instance Show v => ShowN (OTuple v) where
--     getShow _ r = r 
-- 
-- instance Ord v => OrdN (OTuple v) where
--     getOrd _ r =  r 


-- type SimplicialComplex v = DeltaSet (OTuple v)
-- 
-- 
-- simplicialFaces :: forall v. FaceFunction (OTuple v)
-- simplicialFaces = 
-- 
--   (\_i (OT x :: OTuple v (S n)) -> 
--     caseNat4 (undefined :: n)
--                          (OT ($(deleteAtTuple 2) _i x))
--                          (OT ($(deleteAtTuple 3) _i x))
--                          (OT ($(deleteAtTuple 4) _i x))
--                          (OT (lToQuadruple (ldelete (runFI _i) x)))
--                          (\_ -> OT (ldelete (runFI _i) x))
--                          
--                          
--                          )


-- instance (Ord a, Arbitrary a) => Arbitrary (OTuple a N2) where 
--     arbitrary =
--         OT <$> (arbitrary `suchThat` isOrdered3)
-- 
-- 
-- 
--     
-- prop_faces20Ascending xs =
--     unOT xs .=. map3 unOT (faces20Ascending (fromTris [xs] :: SC2 Int) xs)
    
    

-- fromSimplices :: forall v. Ord v => SimpsFunction (OTuple v) -> Dim -> SimplicialComplex v
-- fromSimplices s _dim = 
--     mkDeltaSet 
--         ({-# SCC "fromSimplices/eval:simplicialFaces" #-} simplicialFaces) 
--         ({-# SCC "fromSimplices/eval:s" #-} s) 
--         ({-# SCC "fromSimplices/eval:_dim" #-} _dim) 



fromTets
  :: (Ord v, Show v) => [Quadruple v] -> SC3 v
fromTets = fromOrderedTets . fmap asc4

-- | The -1-dimensional simplicial complex
data SCMinus1 = SCMinus1
    deriving(Show)

data SCCons ts sk = SCCons { 
    sccons_topsimps :: [ts],
    sccons_skeleton :: sk
}
    deriving Show

type SC0 v = SCCons v SCMinus1 
type SC1 v = SCCons (Asc2 v) (SC0 v)
type SC2 v = SCCons (Asc3 v) (SC1 v)
type SC3 v = SCCons (Asc4 v) (SC2 v)

instance Vertices (SC0 v) where
    type Verts (SC0 v) = [v]
    vertices = sccons_topsimps

instance Vertices (SC1 v) where
    type Verts (SC1 v) = [v]
    vertices = vertices . sccons_skeleton

instance Vertices (SC2 v) where
    type Verts (SC2 v) = [v]
    vertices = vertices . sccons_skeleton

instance Vertices (SC3 v) where
    type Verts (SC3 v) = [v]
    vertices = vertices . sccons_skeleton

instance Edges (SC1 v) where
    type Eds (SC1 v) = [Asc2 v]
    edges = sccons_topsimps
    
instance Edges (SC2 v) where
    type Eds (SC2 v) = [Asc2 v]
    edges = edges . sccons_skeleton

instance Edges (SC3 v) where
    type Eds (SC3 v) = [Asc2 v]
    edges = edges . sccons_skeleton

instance Triangles (SC2 v) where
    type Tris (SC2 v) = [Asc3 v]
    triangles = sccons_topsimps

instance Triangles (SC3 v) where
    type Tris (SC3 v) = [Asc3 v]
    triangles = triangles . sccons_skeleton

instance Tetrahedra (SC3 v) where
    type Tets (SC3 v) = [Asc4 v]
    tetrahedra = sccons_topsimps


fromOrderedTets
  :: (Ord v, Show v) => [Asc4 v] -> SC3 v
fromOrderedTets xs = 
    SCCons
        xs
        (fromOrderedTris . nub' . concatMap (asList . subtuplesAsc4_3) $ xs)

fromTris :: (Ord v, Show v) => [Triple v] -> SC2 v
fromTris = fromOrderedTris . map asc3 

fromOrderedTris :: forall v. (Show v, Ord v) => [Asc3 v] -> SC2 v
fromOrderedTris xs = 
    SCCons
        xs
        (fromOrderedEdges . nub' . concatMap (asList . subtuplesAsc3_2) $ xs)


-- | Input tuples need *not* be ordered
fromTrisAndQuads :: (Show v, Ord v) => 
    [Triple v] -> [Quadruple v] -> (SC2 v, Set (Asc2 v))
fromTrisAndQuads (fmap asc3 -> tris) quads = 
        assert (Set.null (Set.intersection quadDiagonals triEdges))
            (sc, quadDiagonals)
    where
        quadHalfs = concatMap (\(a,b,c,d) -> [asc3 (a,b,c),asc3 (a,c,d)]) quads
        quadDiagonals = Set.fromList (fmap (\(a,_,c,_) -> asc2 (a,c)) quads)
        sc = fromOrderedTris (tris ++ quadHalfs)
        triEdges = Set.fromList (edges (fromOrderedTris tris))



fromEdges :: Ord v => [Pair v] -> SC1 v
fromEdges = fromOrderedEdges . map asc2

fromOrderedEdges :: forall v. Ord v => [Asc2 v] -> SC1 v
fromOrderedEdges xs = 
    SCCons 
        xs
        (SCCons
            (nub' . concatMap asList $ xs)
            SCMinus1)


abstractEdge :: SC1 Vertex
abstractEdge = fromEdges [(vA,vB)]

abstractTri :: SC2 Vertex
abstractTri = fromTris [(vA,vB,vC)]

abstractTet :: SC3 Vertex
abstractTet = fromTets [allVertices']







-- oTupleBarycenter :: forall n v. (Nat n, Vector v) => OTuple v n -> v 
-- oTupleBarycenter x = oTupleFoldl1' (&+) x &* (recip (fromIntegral (1 + natToInt (undefined :: n))))
-- 
-- oTupleFoldl1' :: forall n r. Nat n =>  (r -> r -> r) -> OTuple r   n -> r
-- oTupleFoldl1' _c (OT x) = (caseNat4 (undefined :: n)
--     (const id)
--     $(foldl1Tuple 2)
--     $(foldl1Tuple 3)
--     $(foldl1Tuple 4)
--     (\_ -> lfoldl1')
-- 
-- 
--         :: (r -> r -> r) -> OTuple' r n -> r
-- 
--         ) _c x
-- 
-- 
-- anySimplexBarycenter :: Vector v => AnySimplex (OTuple v) -> v
-- anySimplexBarycenter (AnySimplex x) = oTupleBarycenter x
-- 
-- 
-- oTupleMap :: forall n v v'. (Nat n) =>  (v -> v') -> OTuple v   n -> OTuple v'   n
-- oTupleMap f (OT x) = OT (caseNat4 (undefined :: n)
--     (f x)
--     (map2 f x)
--     (map3 f x)
--     (map4 f x)
--     (\_ -> lmap f x))
-- 
-- 
-- 
-- -- coordAllTheThings :: forall v n. Nat n => (OTuple v N0 -> Vec3) -> OTuple v n -> OTuple Vec3 n
-- -- coordAllTheThings f = 
-- --             oTupleMap (f . OT) 
-- 
-- 
--     
-- baryFlat :: forall v. Ord v => (OTuple v N0 -> Vec3) -> (BCSFace (OTuple v) N0 -> Vec3) 
-- 
-- baryFlat f (EP0 (_,AnySimplex x)) = oTupleBarycenter (oTupleMap (f . OT) x)




simplicialFaces10 :: Asc2 v -> Pair v
simplicialFaces10 x = map2 (flip $(deleteAtTuple 2) $ unAsc2 x) ((0,1) :: Pair Int)

simplicialFaces21 :: (Ord v, Show v) => Asc3 v -> Triple (Asc2 v)
simplicialFaces21 x = map3 (unsafeAsc . (flip $(deleteAtTuple 3) $ unAsc3 x)) ((0,1,2) :: Triple Int)

simplicialFaces32
  :: (Ord v, Show v) => Asc4 v -> Quadruple (Asc3 v)
simplicialFaces32 x = map4 (unsafeAsc . (flip $(deleteAtTuple 4) $ unAsc4 x)) ((0,1,2,3) :: Quadruple Int)

instance DeltaSet1 (SC1 v) where faces10 _ = simplicialFaces10 
instance DeltaSet1 (SC2 v) where faces10 _ = simplicialFaces10 
instance DeltaSet1 (SC3 v) where faces10 _ = simplicialFaces10 

instance (Show v, Ord v) => DeltaSet2 (SC2 v) where faces21 _ = simplicialFaces21
instance (Show v, Ord v) => DeltaSet2 (SC3 v) where faces21 _ = simplicialFaces21

instance (Show v, Ord v) => DeltaSet3 (SC3 v) where faces32 _ = simplicialFaces32





instance DisjointUnionable SCMinus1 SCMinus1 SCMinus1 where
    disjointUnion _ _ = SCMinus1


data DJ a b = DJ a b
    deriving Show

instance DisjointUnionable (SCCons ts sk) (SCCons ts' sk') (DJ (SCCons ts sk) (SCCons ts' sk'))
        where
            disjointUnion = DJ 

instance (Vertices s, Vertices s') => Vertices (DJ s s') where
    type Verts (DJ s s') = [Either (Vert s) (Vert s')]

    vertices (DJ s s') = map Left (vertexList s) ++ map Right (vertexList s')  

instance (Edges s, Edges s') => Edges (DJ s s') where
    type Eds (DJ s s') = [Either (Ed s) (Ed s')]

    edges (DJ s s') = map Left (edgeList s) ++ map Right (edgeList s')  

instance (Triangles s, Triangles s') => Triangles (DJ s s') where
    type Tris (DJ s s') = [Either (Tri s) (Tri s')]

    triangles (DJ s s') = map Left (triangleList s) ++ map Right (triangleList s')  

instance (Tetrahedra s, Tetrahedra s') => Tetrahedra (DJ s s') where
    type Tets (DJ s s') = [Either (Tet s) (Tet s')]

    tetrahedra (DJ s s') = map Left (tetrahedronList s) ++ map Right (tetrahedronList s')  

instance (DeltaSet1 s, DeltaSet1 s') => DeltaSet1 (DJ s s') where
    faces10 (DJ s s') = (map2 Left . faces10 s) ||| (map2 Right . faces10 s')

instance (DeltaSet2 s, DeltaSet2 s') => DeltaSet2 (DJ s s') where
    faces21 (DJ s s') = (map3 Left . faces21 s) ||| (map3 Right . faces21 s')

instance (DeltaSet3 s, DeltaSet3 s') => DeltaSet3 (DJ s s') where
    faces32 (DJ s s') = (map4 Left . faces32 s) ||| (map4 Right . faces32 s')


--                 (SCCons ts1 sk1)
--                 (SCCons ts2 sk2)
-- 
--                 =
--                         DJ
--                             (map Left ts1 ++ map Right ts2)
--                             (disjointUnion sk1 sk2)


instance Pretty SCMinus1 where
    pretty _ = text "SC-1"

instance (Pretty ts, Pretty sk) => Pretty (SCCons ts sk) where
    prettyPrec prec (SCCons ts sk) =
        prettyPrecApp prec "SCCons"
            [anyPretty ts, anyPretty sk]
            
instance (Pretty a, Pretty b) => Pretty (DJ a b) where
    prettyPrec prec (DJ a b) = 
        prettyPrecApp prec "DJ"
            [anyPretty a, anyPretty b]

{-# LANGUAGE ViewPatterns, TypeOperators, GADTs, RecordWildCards, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables, NamedFieldPuns, TypeFamilies, DefaultSignatures, FlexibleContexts, OverlappingInstances, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, CPP #-} 
{-# OPTIONS -Wall #-}
-- | All 'Pair's, 'Triple's and 'Quadruple's are assumed to be strictly ascendingly ordered in this module.
module Simplicial.SimplicialComplex where


import AbstractTetrahedron
import Control.Exception
import Control.Monad
import Data.List
import Data.Set as Set
import Data.Vect.Double 
import FaceIx
import HomogenousTuples
import TypeLevel.TF.List
import PreRenderable
import Simplicial.AnySimplex
import Simplicial.DeltaSet
import Simplicial.GraphComplex
--import Simplicial.Labels
import TupleTH
import PrettyUtil


type Plus4 n = S (S (S (S n)))

type family OTuple' v n
type instance (OTuple' v) N0 = v
type instance (OTuple' v) N1 = Pair v
type instance (OTuple' v) N2 = Triple v
type instance (OTuple' v) N3 = Quadruple v
type instance (OTuple' v) (Plus4 n) = List v (S (Plus4 n)) 
                                        
newtype OTuple v n = OT { unOT :: OTuple' v n } 



#define OT_DERIVE(F,OARGS,ARGS)\
    F OARGS = caseNat4 (undefined :: n) (F ARGS) (F ARGS) (F ARGS) (F ARGS) (const (F ARGS))

instance (Show v, Nat n) => Show (OTuple v n) where
    OT_DERIVE( showsPrec prec, (OT x), x )

instance (Pretty v, Nat n) => Pretty (OTuple v n) where
    OT_DERIVE( prettyPrec prec, (OT x), x )

instance (Eq v, Nat n) => Eq (OTuple v n) where
    OT_DERIVE( (==), (OT x) (OT y), x y )

instance (Ord v, Nat n) => Ord (OTuple v n) where
    OT_DERIVE( compare, (OT x) (OT y), x y )


#undef OT_DERIVE

instance Show v => ShowN (OTuple v) where
    getShow _ r = r 

instance Ord v => OrdN (OTuple v) where
    getOrd _ r =  r 


type SimplicialComplex v = DeltaSet (OTuple v)


simplicialFaces :: forall v. FaceFunction (OTuple v)
simplicialFaces = 

  (\_i (OT x :: OTuple v (S n)) -> 
    caseNat4 (undefined :: n)
                         (OT ($(deleteAtTuple 2) _i x))
                         (OT ($(deleteAtTuple 3) _i x))
                         (OT ($(deleteAtTuple 4) _i x))
                         (OT (lToQuadruple (ldelete (runFI _i) x)))
                         (\_ -> OT (ldelete (runFI _i) x))
                         
                         
                         )
    
    
    

fromSimplices :: forall v. Ord v => SimpsFunction (OTuple v) -> Dim -> SimplicialComplex v
fromSimplices s _dim = 
    mkDeltaSet 
        ({-# SCC "fromSimplices/eval:simplicialFaces" #-} simplicialFaces) 
        ({-# SCC "fromSimplices/eval:s" #-} s) 
        ({-# SCC "fromSimplices/eval:_dim" #-} _dim) 



fromTets :: Ord v => [Quadruple v] -> SimplicialComplex v
fromTets = fromOrderedTets . fmap sort4

fromOrderedTets :: forall v. Ord v => [Quadruple v] -> SimplicialComplex v
fromOrderedTets xs = assert (all isOrdered4 xs) $ fromSimplices s (HomogenousDim 3)
    where

        s :: SimpsFunction (OTuple v)
        s = mkSimpsFunction (\n -> caseNat4 n
                        (fmap OT . nub' . concatMap toList4 $ xs)
                        (fmap OT . nub' . concatMap (toList6 . $(subtuples 4 2)) $ xs)
                        (fmap OT . nub' . concatMap (toList4 . $(subtuples 4 3)) $ xs)
                        (fmap OT xs)
                        (const []))


fromTris :: Ord v => [Triple v] -> SimplicialComplex v
fromTris = fromOrderedTris . fmap sort3 

fromOrderedTris :: forall v. Ord v => [Triple v] -> SimplicialComplex v
fromOrderedTris xs = assert (all isOrdered3 xs) $ fromSimplices s (HomogenousDim 2)
    where

        s :: SimpsFunction (OTuple v)
        s = {-# SCC "fromTris/s" #-}
             mkSimpsFunction (\n -> caseNat3 n
                        (fmap OT . nub' . concatMap toList3 $ xs)
                        (fmap OT . nub' . concatMap (toList3 . $(subtuples 3 2)) $ xs)
                        (fmap OT xs)
                        (const []))


-- | Input tuples need *not* be ordered
fromTrisAndQuads :: Ord v => 
    [Triple v] -> [Quadruple v] -> (SimplicialComplex v, Set (OTuple v N1))
fromTrisAndQuads (fmap sort3 -> tris) quads = 
        assert (Set.null (Set.intersection quadDiagonals triEdges))
            (sc, quadDiagonals)
    where
        quadHalfs = concatMap (\(a,b,c,d) -> [sort3 (a,b,c),sort3 (a,c,d)]) quads
        quadDiagonals = Set.fromList (fmap (\(a,_,c,_) -> OT (sort2 (a,c))) quads)
        sc = fromOrderedTris (tris ++ quadHalfs)
        triEdges = Set.fromList (edges (fromOrderedTris tris))



fromEdges :: forall v. Ord v => [Pair v] -> SimplicialComplex v
fromEdges xs = assert (all isOrdered2 xs) $ fromSimplices s (HomogenousDim 1)
    where

        s :: SimpsFunction (OTuple v)
        s = mkSimpsFunction (\n -> caseNat2 n
                        (fmap OT . nub' . concatMap toList2 $ xs)
                        (fmap OT xs)
                        (const []))


abstractEdge :: SimplicialComplex Vertex
abstractEdge = fromEdges [(vA,vB)]

abstractTri :: SimplicialComplex Vertex
abstractTri = fromTris [(vA,vB,vC)]

abstractTet :: SimplicialComplex Vertex
abstractTet = fromTets [allVertices']






tet3d :: PreRenderable (OTuple Vertex)
tet3d = mkPreRenderable (vertexDefaultCoords . unOT) abstractTet 

oTupleBarycenter :: forall n v. (Nat n, Vector v) => OTuple v n -> v 
oTupleBarycenter x = oTupleFoldl1' (&+) x &* (recip (fromIntegral (1 + natToInt (undefined :: n))))

oTupleFoldl1' :: forall n r. Nat n =>  (r -> r -> r) -> OTuple r   n -> r
oTupleFoldl1' _c (OT x) = (caseNat4 (undefined :: n)
    (const id)
    $(foldl1Tuple 2)
    $(foldl1Tuple 3)
    $(foldl1Tuple 4)
    (\_ -> lfoldl1')


        :: (r -> r -> r) -> OTuple' r n -> r

        ) _c x


anySimplexBarycenter :: Vector v => AnySimplex (OTuple v) -> v
anySimplexBarycenter (AnySimplex x) = oTupleBarycenter x


oTupleMap :: forall n v v'. (Nat n) =>  (v -> v') -> OTuple v   n -> OTuple v'   n
oTupleMap f (OT x) = OT (caseNat4 (undefined :: n)
    (f x)
    (map2 f x)
    (map3 f x)
    (map4 f x)
    (\_ -> lmap f x))



-- coordAllTheThings :: forall v n. Nat n => (OTuple v N0 -> Vec3) -> OTuple v n -> OTuple Vec3 n
-- coordAllTheThings f = 
--             oTupleMap (f . OT) 


    
baryFlat :: forall v. Ord v => (OTuple v N0 -> Vec3) -> (BCSFace (OTuple v) N0 -> Vec3) 

baryFlat f (EP0 (_,AnySimplex x)) = oTupleBarycenter (oTupleMap (f . OT) x)

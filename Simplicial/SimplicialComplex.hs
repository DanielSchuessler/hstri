{-# LANGUAGE TypeOperators, GADTs, RecordWildCards, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables, NamedFieldPuns, TypeFamilies, DefaultSignatures, FlexibleContexts, OverlappingInstances, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, CPP #-} 
-- | All 'Pair's, 'Triple's and 'Quadruple's are assumed to be strictly ascendingly ordered in this module.
module Simplicial.SimplicialComplex where


import AbstractTetrahedron
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Graph.Inductive
import Data.List
import Data.Map as Map
import Data.Set as Set
import Data.Vect.Double 
import Data.Vect.Double.Instances
import Debug.Trace
import FaceIx
import HomogenousTuples
import List
import Simplicial.AnySimplex
import Simplicial.DeltaSet
import Simplicial.GraphComplex
import Simplicial.Labels
import TupleTH


type Plus4 n = S (S (S (S n)))

type family OTuple' v n
type instance (OTuple' v) N0 = v
type instance (OTuple' v) N1 = Pair v
type instance (OTuple' v) N2 = Triple v
type instance (OTuple' v) N3 = Quadruple v
type instance (OTuple' v) (Plus4 n) = List v (S (Plus4 n)) 

newtype OTuple v n = OT { unOT :: OTuple' v n } 

instance (Show v, Nat n) => Show (OTuple v n) where
#define r (showsPrec prec x)
    showsPrec prec (OT x) = caseNat4 (undefined :: n) r r r r (const r)
#undef r


instance (Eq v, Nat n) => Eq (OTuple v n) where

instance (Ord v, Nat n) => Ord (OTuple v n) where


instance Show v => ShowN (OTuple v) where
    getShow _ r = r 

instance Ord v => OrdN (OTuple v) where
    getOrd _ r =  r 


type SimplicialComplex v = DeltaSet (OTuple v)


-- | The @v@ parameter is a dummy
simplicialFaces :: forall v. v -> FaceFunction (OTuple v)
simplicialFaces _ = 

  (\i (OT x :: OTuple v (S n)) -> 
    caseNat4 (undefined :: n)
                         (OT ($(deleteAtTuple 2) i x))
                         (OT ($(deleteAtTuple 3) i x))
                         (OT ($(deleteAtTuple 4) i x))
                         (OT (lToQuadruple (ldelete (runFI i) x)))
                         (\_ -> OT (ldelete (runFI i) x))
                         
                         
                         )
    
    
    

fromSimps :: forall v. Ord v => SimpsFunction (OTuple v) -> Dim -> SimplicialComplex v
fromSimps s dim = mkDeltaSet (simplicialFaces (undefined :: v)) s dim

fromTets :: forall v. Ord v => [Quadruple v] -> SimplicialComplex v
fromTets xs = assert (all isOrdered4 xs) $ fromSimps s (HomogenousDim 3)
    where

        s :: SimpsFunction (OTuple v)
        s = mkSimpsFunction (\n -> caseNat4 n
                        (fmap OT . nub' . concatMap toList4 $ xs)
                        (fmap OT . nub' . concatMap (toList6 . $(subtuples 4 2)) $ xs)
                        (fmap OT . nub' . concatMap (toList4 . $(subtuples 4 3)) $ xs)
                        (fmap OT xs)
                        (const []))


fromTris :: forall v. Ord v => [Triple v] -> SimplicialComplex v
fromTris xs = assert (all isOrdered3 xs) $ fromSimps s (HomogenousDim 2)
    where

        s :: SimpsFunction (OTuple v)
        s = mkSimpsFunction (\n -> caseNat3 n
                        (fmap OT . nub' . concatMap toList3 $ xs)
                        (fmap OT . nub' . concatMap (toList3 . $(subtuples 3 2)) $ xs)
                        (fmap OT xs)
                        (const []))


fromEdges :: forall v. Ord v => [Pair v] -> SimplicialComplex v
fromEdges xs = assert (all isOrdered2 xs) $ fromSimps s (HomogenousDim 1)
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






tet3d :: WithCoords (OTuple Vertex)
tet3d = addCoordFunc (f . fromEnum . unOT) (const Nothing) abstractTet
    where
        f 0 = vec3X
        f 1 = vec3Y
        f 2 = vec3Z
        f 3 = Vec3 1 1 1

oTupleBarycenter :: forall n v. (Nat n, Vector v) => OTuple v n -> v 
oTupleBarycenter x = oTupleFoldl1' (&+) x &* (recip (fromIntegral (1 + natToInt (undefined :: n))))

oTupleFoldl1' :: forall n r. Nat n =>  (r -> r -> r) -> OTuple r   n -> r
oTupleFoldl1' c (OT x) = (caseNat4 (undefined :: n)
    (const id)
    $(foldl1Tuple 2)
    $(foldl1Tuple 3)
    $(foldl1Tuple 4)
    (\_ -> lfoldl1')


        :: (r -> r -> r) -> OTuple' r n -> r

        ) c x

anySimplexBarycenter :: Vector v => AnySimplex (OTuple v) -> v
anySimplexBarycenter (AnySimplex x) = oTupleBarycenter x


oTupleMap :: forall n v v'. (Nat n) =>  (v -> v') -> OTuple v   n -> OTuple v'   n
oTupleMap f (OT x) = OT (caseNat4 (undefined :: n)
    (f x)
    (map2 f x)
    (map3 f x)
    (map4 f x)
    (\_ -> lmap f x))



coordAllTheThings :: WithCoords (OTuple v) -> LabeledDeltaSet (OTuple v) (OTuple Vec3)
coordAllTheThings a = 
        LabeledDeltaSet ds 
            (SimplexLabels (oTupleMap (getCoords a . OT))) 


    where
        ds = lds_ds a
    
baryFlat :: forall v. Ord v =>
        WithCoords (OTuple v) -> WithCoords (BCSFace (OTuple v))

baryFlat a = mapSimplexLabels f b
    where
        f :: forall n. Nat n => 
            BCSFace (OTuple Vec3)   n -> CoordLabelsF   n

        f x = caseNat3 (undefined :: n)
                    (case x of
                          EP0 (_,x') -> CoordLabelsF0 (anySimplexBarycenter x'))
                    CoordLabelsF1
                    (trace "TriangleLabels not yet supported for barycentric subdivision"
                        (CoordLabelsF2 Nothing))
                    (const CoordLabelsF3)
                         


        b :: LabeledDeltaSet (BCSFace (OTuple v)) (BCSFace (OTuple Vec3))
        b = bary (coordAllTheThings a)




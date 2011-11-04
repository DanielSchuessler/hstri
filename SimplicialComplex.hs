{-# LANGUAGE TypeOperators, GADTs, RecordWildCards, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables, NamedFieldPuns, TypeFamilies, DefaultSignatures, FlexibleContexts, OverlappingInstances, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, CPP #-} 
-- | All 'Pair's, 'Triple's and 'Quadruple's are assumed to be strictly ascendingly ordered in this module.
module SimplicialComplex where

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
import DeltaSet
import GraphComplex
import HomogenousTuples
import List
import SimplexLabels
import TupleTH
import TypeLevel.TF
import FaceIx

isOrdered2 (v0,v1) = v0 < v1
isOrdered3 (v0,v1,v2) = v0 < v1 && v1 < v2 
isOrdered4 (v0,v1,v2,v3) = isOrdered3 (v0,v1,v2) && v2 < v3

toList6 :: (a,a,a,a,a,a) -> [a]
toList6 = $(tupleToList 6)



data OTuple v 

type Plus4 n = S (S (S (S n)))

type instance (OTuple v) :$ N0 = v
type instance (OTuple v) :$ N1 = Pair v
type instance (OTuple v) :$ N2 = Triple v
type instance (OTuple v) :$ N3 = Quadruple v
type instance (OTuple v) :$ Plus4 n = List v (S (Plus4 n)) 


instance Show v => ShowN (OTuple v) where
    getShow _ n r = caseNat4 n r r r r (const r)

instance Ord v => OrdN (OTuple v) where
    getOrd _ n r =  caseNat4 n r r r r (const r)


type SimplicialComplex v = DeltaSet (OTuple v)


-- | The @v@ parameter is a dummy
simplicialFaces :: v -> FaceFunction (OTuple v)
simplicialFaces _ n = caseNat4 n
                         $(deleteAtTuple 2)
                         $(deleteAtTuple 3)
                         $(deleteAtTuple 4)
                         (\i x -> lToQuadruple (ldelete (runFI i) x))
                         (\_ i x -> ldelete (runFI i) x)

fromSimps :: forall v. Ord v => SimpsFunction (OTuple v) -> Dim -> SimplicialComplex v
fromSimps s dim = mkDeltaSet (simplicialFaces (undefined :: v)) s dim

fromTets :: forall v. Ord v => [Quadruple v] -> SimplicialComplex v
fromTets xs = assert (all isOrdered4 xs) $ fromSimps s (HomogenousDim 3)
    where

        s :: SimpsFunction (OTuple v)
        s n = caseNat4 n
                        (nub' . concatMap toList4 $ xs)
                        (nub' . concatMap (toList6 . $(subtuples 4 2)) $ xs)
                        (nub' . concatMap (toList4 . $(subtuples 4 3)) $ xs)
                        xs
                        (const [])


fromTris :: forall v. Ord v => [Triple v] -> SimplicialComplex v
fromTris xs = assert (all isOrdered3 xs) $ fromSimps s (HomogenousDim 2)
    where

        s :: SimpsFunction (OTuple v)
        s n = caseNat3 n
                        (nub' . concatMap toList3 $ xs)
                        (nub' . concatMap (toList3 . $(subtuples 3 2)) $ xs)
                        (xs)
                        (const [])


fromEdges :: forall v. Ord v => [Pair v] -> SimplicialComplex v
fromEdges xs = assert (all isOrdered2 xs) $ fromSimps s (HomogenousDim 1)
    where

        s :: SimpsFunction (OTuple v)
        s n = caseNat2 n
                        (nub' . concatMap toList2 $ xs)
                        (xs)
                        (const [])


abstractEdge :: SimplicialComplex Vertex
abstractEdge = fromEdges [(vA,vB)]

abstractTri :: SimplicialComplex Vertex
abstractTri = fromTris [(vA,vB,vC)]

abstractTet :: SimplicialComplex Vertex
abstractTet = fromTets [allVertices']






tet3d :: WithCoords (OTuple Vertex)
tet3d = addCoordFunc (f . fromEnum) abstractTet
    where
        f 0 = vec3X
        f 1 = vec3Y
        f 2 = vec3Z
        f 3 = Vec3 1 1 1

oTupleBarycenter :: forall n v. (Nat n, Vector v) => n -> OTuple v :$ n -> v 
oTupleBarycenter n x = oTupleFoldl1' n (&+) x &* (recip (fromIntegral (1 + natToInt (undefined :: n))))

oTupleFoldl1' :: forall n r. Nat n => n -> (r -> r -> r) -> OTuple r :$ n -> r
oTupleFoldl1' n = caseNat4 n
    (const id)
    $(foldl1Tuple 2)
    $(foldl1Tuple 3)
    $(foldl1Tuple 4)
    (\_ -> lfoldl1')

anySimplexBarycenter :: Vector v => AnySimplex (OTuple v) -> v
anySimplexBarycenter (AnySimplex n x) = oTupleBarycenter n x


oTupleMap :: (Nat n) => n -> (v -> v') -> OTuple v :$ n -> OTuple v' :$ n
oTupleMap n = caseNat4 n
    ($)
    map2
    map3
    map4
    (\_ -> lmap)



coordAllTheThings :: WithCoords (OTuple v) -> LabeledDeltaSet (OTuple v) (OTuple Vec3)
coordAllTheThings a = 
        LabeledDeltaSet ds 
            (SimplexLabels (\n -> oTupleMap n (vertlbl a))) 


    where
        ds = lds_ds a
    
baryFlat :: forall v. Ord v =>
        WithCoords (OTuple v) -> WithCoords (BCSFace' (OTuple v))

baryFlat a = mapSimplexLabels f b
    where
        f :: forall n. Nat n => 
            n -> BCSFace' (OTuple Vec3) :$ n -> OneElSequence Vec3 N0 :$ n

        f n x = caseNat n
                    (case x of
                          EP0 (_,x') -> anySimplexBarycenter x')
                    (const ())


        b :: LabeledDeltaSet (BCSFace' (OTuple v)) (BCSFace' (OTuple Vec3))
        b = bary (coordAllTheThings a)




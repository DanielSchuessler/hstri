{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module DeltaSet where

import HomogenousTuples
import Data.Map as Map
import Element
import Control.Applicative
import Control.Arrow
import Control.Monad
import Nat
import Data.Graph.Inductive
import Data.Typeable
import Control.Exception
import Data.Maybe

#include "macros.h"



data RightShift f
type instance RightShift f :$ n = f :$ (S n)

data Sequence f = Sequence (f :$ N0) (Sequence (RightShift f)) 

sequenceElem :: NaturalNumber n => n -> Sequence f -> f :$ n
sequenceElem n = caseN n
    (\(Sequence x _) -> x)
    (\n' (Sequence _ xs) -> sequenceElem n' xs)

toSequence :: forall f. (forall n. NaturalNumber n => n -> f :$ n) -> Sequence f
toSequence f = Sequence (f n0) (toSequence f')
    where
        f' :: forall n. NaturalNumber n => n -> RightShift f :$ n
        f' _ = f (undefined :: S n)





newtype FaceIx = FI Int
    deriving(Num,Enum,Show,Eq,Ord)

runFI :: FaceIx -> Int
runFI (FI i) = i

type FaceFunction a  = forall n. NaturalNumber n => n -> FaceIx -> (a :$ (S n)) -> (a :$ n)
type SimpsFunction a = forall n. NaturalNumber n => n -> [a :$ n]
type SuperFunction a = forall n. NaturalNumber n => n -> (a :$ n) -> [a :$ (S n)]

data Dim = 
    -- | Each simplex is contained in a maximal simplex of the given dimension
    HomogenousDim Int |
    -- | The int specifies the maximum dimension
    InhomogenousDim Int
    deriving(Show)

type FaceGraph a = Gr (AnySimplex a) FaceIx

class OrdN a => DeltaSet a where
    type SimplexSequence a 

    face :: a -> FaceFunction a
    simps :: a -> SimpsFunction a
    supers :: a -> SuperFunction a
    dimension :: a -> Dim

    -- | Convention: Edges are directed from subfaces to superfaces
    faceGraph :: a -> FaceGraph a

    faceGraph a = mkGraph 
        ((\(x,y) -> (y,x)) <$> Map.assocs nodeMap)
        (do
            (AnySimplex n x, xnode) <- Map.assocs nodeMap
            caseN n
                []
                (\n' -> do
                    i <- [0..maxFaceIx n]
                    let 
                        y = face a n' i x
                        ynode = nodeMap ! AnySimplex n' y
                    return (ynode, xnode, i))
        )




        where
            nodeMap :: Map (AnySimplex a) Node
            nodeMap = Map.fromList (zip (allSimplices a) [0::Node ..]) 



type Vert a = a :$ N0
type Arc a =  a :$ N1
type Tri a =  a :$ N2
type Tet a =  a :$ N3


data DS s = DS {
    -- | The first component of the result must be the face obtained by leaving out the first vertex, and so on
    face_   :: FaceFunction s,
    simps_  :: SimpsFunction s,
    supers_ :: SuperFunction s,
    dimension_ :: Dim,
    faceGraph_ :: FaceGraph (DS s)
}

type instance DS a :$ n = a :$ n

-- -- | This is more or less the identity 
-- joinDS :: DS (DS a) -> DS a
-- joinDS DS{..} = DS{..}

instance OrdN a => DeltaSet (DS a) where
    face = face_
    simps = simps_
    supers = supers_
    dimension = dimension_
    faceGraph = faceGraph_

faces10 :: DeltaSet a => a -> Arc a -> Pair (Vert a)
faces10 a x = map2 (\i -> face a n0 i x) (0,1)

faces21 :: DeltaSet a => a -> Tri a -> Triple (Arc a)
faces21 a x = map3 (\i -> face a n1 i x) (0,1,2)

faces32 :: DeltaSet a => a -> Tet a -> Quadruple (Tri a)
faces32 a x = map4 (\i -> face a n2 i x) (0,1,2,3)

s0 :: DeltaSet a => a -> [Vert a]
s0 = flip simps n0
s1 :: DeltaSet a => a -> [Arc a]
s1 = flip simps n1
s2 :: DeltaSet a => a -> [Tri a]
s2 = flip simps n2
s3 :: DeltaSet a => a -> [Tet a]
s3 = flip simps n3

super01 :: DeltaSet a => a -> Vert a -> [Arc a]
super01 = flip supers n0

super12 :: DeltaSet a => a -> Arc a -> [Tri a]
super12 = flip supers n1

super23 :: DeltaSet a => a -> Tri a -> [Tet a]
super23 = flip supers n2


faces20 :: DeltaSet a => a -> Tri a -> (Vert a, Vert a, Vert a)
faces20 t tr = (v2,v1,v0)
    where
        (e12,e02,_) = faces21 t tr
        (v2,v1) = faces10 t e12
        (_,v0) = faces10 t e02

faces31
  :: DeltaSet a =>
     a
     -> Tet a
     -> (Arc a, Arc a, Arc a, Arc a, Arc a, Arc a)
faces31 t tet = (e23,e13,e12,e03,e02,e01)
    where
        (tr123,tr023,tr013,_) = faces32 t tet
        (e23,e13,e12) = faces21 t tr123
        ( _ ,e03,e02) = faces21 t tr023
        ( _ , _ ,e01) = faces21 t tr013

faces30
  :: DeltaSet a => a -> Tet a -> (Vert a, Vert a, Vert a, Vert a)
faces30 t tet = (v3,v2,v1,v0)
    where
        (tr123,tr023,_,_) = faces32 t tet
        (v3,v2,v1) = faces20 t tr123
        ( _, _,v0) = faces20 t tr023


-- s0_default t = Set.unions (fmap f (s3 t))
--     where
--         f = Set.fromList . $(tupleToList 4) . faces30 t
-- 
-- s1_default t = Set.unions (fmap f (s3 t))
--     where
--         f = Set.fromList . $(tupleToList 6) . faces31 t
-- 
-- s2_default t = Set.unions (fmap f (s3 t))
--     where
--         f = Set.fromList . $(tupleToList 4) . faces32 t






memoFun :: Ord k => (k -> a) -> [k] -> Map k a
memoFun f xs = Map.fromList [(x,f x) | x <- xs] 

memoSuper
  :: forall a. (DeltaSet a, Ord (Vert a), Ord (Arc a), Ord (Tri a)) => a -> DS a
memoSuper d = 
    let
            super01_ = (memoFun (super01 d) (s0 d) !)
            super12_ = (memoFun (super12 d) (s1 d) !)
            super23_ = (memoFun (super23 d) (s2 d) !)

            supers_ :: SuperFunction a
            supers_ n = caseN3 n
                            super01_
                            super12_
                            super23_
                            (const $ error ("'supers' called at dimension > 2 for a memoSuper'd complex"))

            face_ :: FaceFunction a
            face_ = face d
            simps_ :: SimpsFunction a
            simps_ = simps d
            dimension_ = dimension d
    in
        DS { .. }
    

instance (Show (s :$ N0), Show (s :$ N1), Show (s :$ N2), Show (s :$ N3)) => Show (DS s) where
    show a = 
                    concat [ 
                        f (s3 a) (asList . faces32 a), 
                        f (s2 a) (asList . faces21 a), 
                        f (s1 a) (asList . faces10 a),
                        showVerts
                        ]
        where
            f :: (Show a, Show b) => [a] -> (a -> [b]) -> String
            f cs _faces = unlines $ do
                c <- cs
                (show c ++ " -> ") : do
                    b <- _faces c
                    ["    " ++ show b]

            showVerts | HomogenousDim n <- dimension_ a, n > 0 = ""
                      | otherwise = 
                            unlines (fmap show (s0 a)) 

data FaceIndexOutOfBounds = FaceIndexOutOfBounds FaceIx deriving(Show,Typeable)
instance Exception FaceIndexOutOfBounds
                    

        

dimMax :: Dim -> Int
dimMax d = case d of
                      HomogenousDim n -> n
                      InhomogenousDim n -> n

maxDimension :: DeltaSet a => a -> Int
maxDimension = dimMax . dimension


data DisjointUnion a b = DisjointUnion a b
    deriving (Show)

type instance (DisjointUnion a b) :$ n = Either (a :$ n) (b :$ n)

instance (DeltaSet a, DeltaSet b) => DeltaSet (DisjointUnion a b) where

    dimension (DisjointUnion a b) =
                case (dimension a, dimension b) of
                     (hda@(HomogenousDim da), (HomogenousDim db)) | da == db -> hda
                     (da, db) -> InhomogenousDim (max (dimMax da) (dimMax db))


     --       face_ :: FaceFunction (SimplexTypeSequence (DisjointUnion a b))
    face (DisjointUnion a b) n i = 
                face a n i +++ face b n i
    --        simps_ :: SimpsFunction (SimplexTypeSequence (DisjointUnion a b))
    simps (DisjointUnion a b) n = 
                (Left <$> simps a n) ++ (Right <$> simps b n)
     --       supers_ :: SuperFunction (SimplexTypeSequence (DisjointUnion a b))
    supers (DisjointUnion a b) n = 
                (fmap Left  . supers a n) ||| 
                (fmap Right . supers b n)



foldMapDJ
  :: (t3 -> t4 -> t)
     -> (t1 -> t3) -> (t2 -> t4) -> DisjointUnion t1 t2 -> t
foldMapDJ g f f' (DisjointUnion a b) = g (f a) (f' b)

deriveDJ :: (a -> a' -> r) -> (b -> b' -> r) -> DisjointUnion a b -> Either a' b' -> r
deriveDJ = foldMapDJ (|||)


data ComplexPlus a b = ComplexPlus {
    cp_complex :: a,
    cp_extra :: b
}
    deriving Show

DERIVE_DELTA_SET(ComplexPlus a b, cp_complex) 




instance ShowN v => ShowN (DS v) where
    getShow _ = getShow (undefined :: v)
instance OrdN v => OrdN (DS v) where
    getOrd _ = getOrd (undefined :: v)



data AnySimplex a = forall n. NaturalNumber n => AnySimplex n (a :$ n)

instance ShowN a => Show (AnySimplex a) where
    showsPrec prec (AnySimplex n x) =
        showsPrecN (undefined :: a) n prec x

instance OrdN a => Eq (AnySimplex a) where
    a == b = compare a b == EQ

instance OrdN a => Ord (AnySimplex a) where
    compare (AnySimplex n1_ x1) (AnySimplex n2_ x2) =
        case natEq n1_ n2_ of
             Just Refl -> getOrd (undefined :: a) n1_ (compare x1 x2)
             _ -> compare (naturalNumberAsInt n1_) (naturalNumberAsInt n2_)
            

    

mapDimensions :: forall a r. DeltaSet a => (forall n. NaturalNumber n => n -> r) -> a -> [r]
mapDimensions f a = go (maxDimension a) n0
    where
        go :: NaturalNumber n => Int -> n -> [r]
        go i n | i < 0 = []
               | otherwise = f n : go (i-1) (successorTo n)

allSimplices :: DeltaSet a => a -> [AnySimplex a]
allSimplices a = concat $ mapDimensions (\n -> AnySimplex n <$> simps a n) a 

maxFaceIx :: NaturalNumber n => n -> FaceIx
maxFaceIx = FI . naturalNumberAsInt

faces :: (NaturalNumber n, DeltaSet a) => a -> n -> (a :$ S n) -> [a :$ n]
faces a n x = [ face a n i x | i <- [0..maxFaceIx n] ] 






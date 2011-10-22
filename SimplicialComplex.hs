{-# LANGUAGE TypeOperators, GADTs, RecordWildCards, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, ScopedTypeVariables, NamedFieldPuns, TypeFamilies, DefaultSignatures, FlexibleContexts, OverlappingInstances, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving, CPP #-} 
-- | All 'Pair's, 'Triple's and 'Quadruple's are assumed to be strictly ascendingly ordered in this module.
module SimplicialComplex where

import AbstractTetrahedron
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.Map as Map
import Data.Set as Set
import Data.Vect.Double 
import Data.Vect.Double.Instances
import DeltaSet
import HomogenousTuples
import Nat
import TupleTH
import Data.Graph.Inductive
import SimplexLabels

#include "macros.h"

isOrdered2 (v0,v1) = v0 < v1
isOrdered3 (v0,v1,v2) = v0 < v1 && v1 < v2 
isOrdered4 (v0,v1,v2,v3) = isOrdered3 (v0,v1,v2) && v2 < v3

toList6 :: (a,a,a,a,a,a) -> [a]
toList6 = $(tupleToList 6)



data OrdTuple v 

type instance (OrdTuple v) :$ N0 = v
type instance (OrdTuple v) :$ N1 = Pair v
type instance (OrdTuple v) :$ N2 = Triple v
type instance (OrdTuple v) :$ N3 = Quadruple v

type SC v = DS (OrdTuple v)

simplicialSuper01 :: Eq v => [Pair v] -> v -> [Pair v]
simplicialSuper01 s1_ = \v -> Prelude.filter ($(elemTuple 2) v) s1_
simplicialSuper12 :: Eq v => [Triple v] -> Pair v -> [Triple v]
simplicialSuper12 s2_ = \e -> Prelude.filter (\t -> $(elemTuple 3) e ($(subtuples 3 2) t)) s2_
simplicialSuper23 :: Eq v => [Quadruple v] -> Triple v -> [Quadruple v]
simplicialSuper23 s3_ = \t -> Prelude.filter (\tt -> $(elemTuple 4) t ($(subtuples 4 3) tt)) s3_

-- | The @v@ parameter is a dummy
simplicialSupers :: (Eq v) => v -> SimpsFunction (OrdTuple v) -> SuperFunction (OrdTuple v)
simplicialSupers _ s n = caseN3 n
                        (simplicialSuper01 (s n1))
                        (simplicialSuper12 (s n2))
                        (simplicialSuper23 (s n3))
                        undefined



-- simplicialFaces10_ (v0,v1) = (v1,v0)
-- simplicialFaces21_ (v0,v1,v2) = ((v1,v2),(v0,v2),(v1,v2))
-- simplicialFaces32_ (v0,v1,v2,v3) = ((v1,v2,v3),(v0,v2,v3),(v0,v1,v3),(v0,v1,v2))

-- | The @v@ parameter is a dummy
simplicialFaces :: v -> FaceFunction (OrdTuple v)
simplicialFaces _ n = caseN3 n
                         $(deleteAtTuple 2)
                         $(deleteAtTuple 3)
                         $(deleteAtTuple 4)
                         undefined

fromSimps :: forall v. Ord v => SimpsFunction (OrdTuple v) -> Dim -> SC v
fromSimps s dim = joinDS . memoSuper $ ds
    where
        ds = DS
                 (simplicialFaces (undefined :: v))    
                 s                                     
                 (simplicialSupers (undefined :: v) s) 
                 dim

fromTets :: forall v. Ord v => [Quadruple v] -> SC v
fromTets xs = assert (all isOrdered4 xs) $ fromSimps s (HomogenousDim 3)
    where

        s :: SimpsFunction (OrdTuple v)
        s n = caseN4 n
                        (nub' . concatMap toList4 $ xs)
                        (nub' . concatMap (toList6 . $(subtuples 4 2)) $ xs)
                        (nub' . concatMap (toList4 . $(subtuples 4 3)) $ xs )
                        xs
                        (const [])


fromTris :: forall v. Ord v => [Triple v] -> SC v
fromTris xs = assert (all isOrdered3 xs) $ fromSimps s (HomogenousDim 2)
    where

        s :: SimpsFunction (OrdTuple v)
        s n = caseN3 n
                        (nub' . concatMap toList3 $ xs)
                        (nub' . concatMap (toList3 . $(subtuples 3 2)) $ xs)
                        (xs)
                        (const [])


fromEdges :: forall v. Ord v => [Pair v] -> SC v
fromEdges xs = assert (all isOrdered2 xs) $ fromSimps s (HomogenousDim 1)
    where

        s :: SimpsFunction (OrdTuple v)
        s n = caseN2 n
                        (nub' . concatMap toList2 $ xs)
                        (xs)
                        (const [])








abstractTet :: SC Vertex
abstractTet = fromTets [allVertices']

data BarySimplex a n where

    Barycenter :: 
        NaturalNumber n => 
            n -> 
            a:$n -> 
            BarySimplex a N0 

    BarySimplex :: 
        NaturalNumber n =>
        BarySimplex a n -> 
        BarySimplex a N0 ->
        BarySimplex a (SuccessorTo n) 


data Bary a = Bary a

type instance Bary a :$ n = BarySimplex a n

instance Show v => ShowN (OrdTuple v) where
    getShow _ n r = caseN4 n r r r r undefined

instance Ord v => OrdN (OrdTuple v) where
    getOrd _ n r =  caseN4 n r r r r undefined 




instance ShowN a => Show (BarySimplex a n) where
    showsPrec prec (Barycenter n x) = 
        showParen (prec>10) $
        (showString "Barycenter "
         . showsPrec 11 (naturalNumberAsInt n) 
         . showChar ' '
         . showsPrecN (undefined::a) n 11 x)

    showsPrec prec (BarySimplex w b) = 
        showParen (prec>10) $
    
        (showString "BarySimplex "
         . showsPrec 11 w
         . showChar ' '
         . showsPrec 11 b)




baryVertices :: forall a. DeltaSet a => a -> [BarySimplex a N0]
baryVertices a = go n0
            where
                m = maxDimension a               

                go :: forall n'. NaturalNumber n' => n' -> [BarySimplex a N0]
                go n' | m < naturalNumberAsInt n' = []
                go n' = (Barycenter n' <$> simps a n') ++ go (successorTo n')

newtype BarySubdivideIH a n = BSIH { runBSIH :: a:$n -> [BarySimplex a n] }

bsih_n :: BarySubdivideIH a n -> n
bsih_n = const undefined

barySubdivide :: (NaturalNumber n, DeltaSet a) => 
    a -> (a:$n) -> [BarySimplex a n]
barySubdivide a = runBSIH 
    (induction'
        (BSIH (\x -> [Barycenter n0 x]))
        (\subdivideFace -> BSIH $ \x -> do 
            let n = bsih_n subdivideFace
            i <- [0..naturalNumberAsInt n+1]
            bs <- runBSIH subdivideFace (face a n i x) 
            return (BarySimplex bs (Barycenter (successorTo n) x))))
                    
-- | Typing helper
barySubdivide' :: (NaturalNumber n, DeltaSet a) => 
    a -> n -> (a:$n) -> [BarySimplex a n]
barySubdivide' a _ = barySubdivide a

newtype BaryFaceIH a n = 
    BFIH { runBFIH :: Int -> BarySimplex a (SuccessorTo n) -> BarySimplex a n }
        
baryFace :: forall a. DeltaSet a => a -> FaceFunction (Bary a)
baryFace a n = runBFIH it
    where
        it = induction'
                (BFIH (\i (BarySimplex base apexBarycenter) ->
                            case i of
                                 0 -> base
                                 1 -> apexBarycenter)) 
                (\(faceOfFace :: BaryFaceIH a n) ->
                    BFIH (\i (BarySimplex baseFace apexBarycenter) ->
                            case i of
                                 0 -> baseFace
                                 _ -> BarySimplex baseFace' apexBarycenter
                                   where
                                      baseFace' = runBFIH faceOfFace (i-1) baseFace
                         
                         
                         ))


newtype BarySupersIH a n = 
    BSuIH { runBSuIH :: BarySimplex a n -> [BarySimplex a (SuccessorTo n)] }


-- baryFace :: forall a. DeltaSet a => a -> SuperFunction (Bary a)
-- baryFace a n = runBsuIH it
--   where
--       it = induction'
--         (BSuIH (\x -> 
            

-- barySimplices :: (DeltaSet a) => a -> SimpsFunction (Bary a)
-- 
-- barySimplices a n = 
--     case asN' n of
--          NZero -> baryVertices a
--          NSuccessorTo _ ->  





#if 0
data BaryVertex1 v = Bary0 v
                   | Bary1 (Pair v)
                    deriving(Eq,Ord,Show,Functor)

data BaryVertex2 v = BaryVertex1 (BaryVertex1 v)
                   | Bary2 (Triple v)
                    deriving(Eq,Ord,Show,Functor)

data BaryVertex3 v = BaryVertex2 (BaryVertex2 v)
                   | Bary3 (Quadruple v)

                    deriving(Eq,Ord,Show,Functor)

class (DeltaSet a, 
       Arc a ~ (Pair (Vert a)), 
       Tri a ~ (Triple (Vert a)), 
       Tet a ~ (Quadruple (Vert a)))

      => SimplicialComplex a

instance (DeltaSet a, 
       Arc a ~ (Pair (Vert a)), 
       Tri a ~ (Triple (Vert a)), 
       Tet a ~ (Quadruple (Vert a)))

      => SimplicialComplex a


bary3 :: (Ord v, SimplicialComplex a, v ~ Vert a) => 
    a -> ComplexPlus (SC (BaryVertex3 v)) a

bary3 c0 = ComplexPlus c c0 
    where
        c =
            fromTets [ $(catTuples 3 1)  
                        ( map3 BaryVertex2 $ $(catTuples 2 1)  
                                (map2 BaryVertex1 (Bary0 v, Bary1 e)) 
                                (Bary2 t)) 
                            (Bary3 tt)
                    |
                        v <- s0 c0,
                        e <- super01 c0 v,
                        t <- super12 c0 e,
                        tt <- super23 c0 t]

bary2 :: (Ord v, SimplicialComplex a, v ~ Vert a) => 
    a -> ComplexPlus (SC (BaryVertex2 v)) a

bary2 c0 = ComplexPlus c c0 
    where
        c =
            fromTris [ 
                        ( $(catTuples 2 1)  
                                (map2 BaryVertex1 (Bary0 v, Bary1 e)) 
                                (Bary2 t)) 
                    |
                        v <- s0 c0,
                        e <- super01 c0 v,
                        t <- super12 c0 e
                        ]


tet3d = addSimplexLabels n0 (vlbl . fromEnum) abstractTet
      where
        vlbl 0 = vec3X
        vlbl 1 = vec3Y
        vlbl 2 = vec3Z
        vlbl 3 = 1


-- instance (v ~ Vert a, SimplexLabels a N0, SimplexLabel a N0 ~ Vec3) => 
--     Coordinates (ComplexPlus (SC (BaryVertex3 v)) a) where
-- 
--     coords (ComplexPlus _ a) v = barycenter (fmap (coords a) v) 
-- 
-- instance (v ~ Vert a, Coordinates a) => Coordinates (ComplexPlus (SC (BaryVertex2 v)) a) where
--     coords (ComplexPlus _ a) v = barycenter (fmap (coords a) v) 


-- 
-- -- 
-- --
-- -- 
-- -- baryGeom :: (Ord v, Vector vlbl) => SimplicialComplex3 v vlbl -> SimplicialComplex3 (BaryVertex v) vlbl
-- -- baryGeom s@SimplicialComplex3 {vlbl} = (bary s) { vlbl = barycenter . fmap vlbl }
-- -- 
-- --
-- 


class Barycenter x where
    barycenter :: x -> Vec3

instance Barycenter (BaryVertex1 Vec3) where
    barycenter (Bary0 v) = v
    barycenter (Bary1 (v0,v1)) = (1/2) *& (v0 &+ v1)

instance Barycenter (BaryVertex2 Vec3) where
    barycenter (BaryVertex1 x) = barycenter x
    barycenter (Bary2 (v0,v1,v2)) = (1/3) *& (v0 &+ v1 &+ v2)

instance Barycenter (BaryVertex3 Vec3) where
    barycenter (BaryVertex2 x) = barycenter x
    barycenter (Bary3 (v0,v1,v2,v3)) = (1/4) *& (v0 &+ v1 &+ v2 &+ v3)

edgeCoords c = map2 (simplbl c n0) <$> s1 c

triCoords c = map3 (simplbl c n0) <$> s2 c

#endif

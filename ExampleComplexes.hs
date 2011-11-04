{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonoLocalBinds, NoMonoPatBinds #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module ExampleComplexes where

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Char
import Data.Graph.Inductive
import Data.Map as Map
import Data.Maybe
import Data.Typeable
import DeltaSet
import Element
import GraphComplex
import HomogenousTuples
import SimplicialComplex
import TypeLevel.TF
import FaceIx
import Test.QuickCheck



data Moebius n where
    Moebius0 :: Moebius N0
    MoebiusBoundary :: Moebius N1 
    MoebiusInner :: Moebius N1 
    Moebius2 :: Moebius N2


type Moebius' = ApplyConstr Moebius


deriving instance Show (Moebius n)
deriving instance Eq (Moebius n)
deriving instance Ord (Moebius n)
instance ShowN Moebius' where getShow _ _ r = r
instance OrdN Moebius' where getOrd _ _ r = r

ix1 :: FaceIx -> a -> a -> a
ix1 i a b = case i of
                   0 -> a
                   1 -> b
                   _ -> throw (FaceIndexOutOfBounds i)

ix2 :: FaceIx -> a -> a -> a -> a
ix2 i a b c = case i of
                   0 -> a
                   1 -> b
                   2 -> c
                   _ -> throw (FaceIndexOutOfBounds i)

moebius :: DeltaSet Moebius'
moebius = mkDeltaSet face_ simps_ dimension_
  where
    dimension_ = HomogenousDim 2

    face_ :: FaceFunction Moebius'
    face_ n i _ = caseNat2 n
                    Moebius0
                    (ix2 i MoebiusInner MoebiusInner MoebiusBoundary)
                    (\_ -> error "impossible")


    simps_ :: SimpsFunction Moebius'
    simps_ n = caseNat3 n
                     [Moebius0]
                     [MoebiusInner, MoebiusBoundary]
                     [Moebius2]
                     (const [])





data Torus n where
    Torus0 :: Torus N0

    TorusDiagEdge :: Torus N1 
    TorusVerticalEdge :: Torus N1 
    TorusHorizontalEdge :: Torus N1 

    TorusLowerFace :: Torus N2
    TorusUpperFace :: Torus N2

type Torus' = ApplyConstr Torus

deriving instance Show (Torus n)
deriving instance Eq (Torus n)
deriving instance Ord (Torus n)
instance ShowN Torus' where getShow _ _ r = r
instance OrdN Torus' where getOrd _ _ r = r


torus :: DeltaSet Torus'
torus = mkDeltaSet face_ simps_ dimension_
  where
    dimension_ = HomogenousDim 2

    face_ :: FaceFunction Torus'
    face_ n i x = caseNat2 n
                    Torus0
                    (case x of
                          TorusLowerFace -> ix2 i TorusVerticalEdge TorusDiagEdge TorusHorizontalEdge
                          TorusUpperFace -> ix2 i TorusHorizontalEdge TorusVerticalEdge TorusDiagEdge)
                              
                    (\_ -> error "impossible")


    simps_ :: SimpsFunction Torus'
    simps_ n = caseNat3 n
                     [Torus0]
                     [TorusDiagEdge,TorusVerticalEdge,TorusHorizontalEdge]
                     [TorusUpperFace,TorusLowerFace]
                     (const [])

data ConeDisk n where
    ConeDiskBaseVertex :: ConeDisk N0 
    ConeDiskApex :: ConeDisk N0 

    ConeDiskBaseCircle :: ConeDisk N1
    ConeDiskUpArc :: ConeDisk N1

    ConeDiskTri :: ConeDisk N2

type ConeDisk' = ApplyConstr ConeDisk

deriving instance Show (ConeDisk n)
deriving instance Eq (ConeDisk n)
deriving instance Ord (ConeDisk n)
instance ShowN ConeDisk' where getShow _ _ r = r
instance OrdN ConeDisk' where getOrd _ _ r = r


coneDisk :: DeltaSet ConeDisk'
coneDisk = mkHomogenousDeltaSet n2 face_ [ConeDiskTri]
  where

    face_ :: FaceFunction ConeDisk'
    face_ n i x = caseNat2 n
                    (case x of
                        ConeDiskBaseCircle -> ConeDiskBaseVertex                        
                        ConeDiskUpArc -> ix1 i ConeDiskApex ConeDiskBaseVertex)

                    (ix2 i ConeDiskUpArc ConeDiskUpArc ConeDiskBaseCircle)
                              
                    (\_ -> error "impossible")





linearGraph :: Node -> Gr Char String
linearGraph 0 = mkGraph [(0,'a')] []
linearGraph n = cxt & linearGraph n'
    where 
        n' = n-1
        cxt = ([( [c(n-1),c n]  ,n-1)],n,c n,[])
        c m = chr (ord 'a' + m)



{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module ExampleComplexes where

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
import DeltaSet

#include "macros.h"

data Moebius = Moebius deriving Show

data MoebiusS n where
    Moebius0 :: MoebiusS N0
    MoebiusBoundary :: MoebiusS N1 
    MoebiusInner :: MoebiusS N1 
    Moebius2 :: MoebiusS N2

type instance Moebius :$ n = MoebiusS n

deriving instance Show (MoebiusS n)
deriving instance Eq (MoebiusS n)
deriving instance Ord (MoebiusS n)
instance ShowN Moebius where getShow _ _ r = r
instance OrdN Moebius where getOrd _ _ r = r


instance DeltaSet Moebius where
    dimension _ = HomogenousDim 2
    face _ n i = caseN2 n
                    (const Moebius0)
                    (\_ -> case i of
                                               0 -> MoebiusInner
                                               1 -> MoebiusInner
                                               2 -> MoebiusBoundary
                                               _ -> throw (FaceIndexOutOfBounds i))
                    (\_ _ -> error "impossible")


    simps _ n = caseN3 n
                     [Moebius0]
                     [MoebiusInner, MoebiusBoundary]
                     [Moebius2]
                     (const [])


    supers a n _ = simps a (successorTo n)


data Torus = Torus deriving Show

data TorusS n where
    Torus0 :: TorusS N0
    TorusDiagEdge :: TorusS N1 
    TorusVerticalEdge :: TorusS N1 
    TorusHorizontalEdge :: TorusS N1 
    TorusLowerFace :: TorusS N2
    TorusUpperFace :: TorusS N2

type instance Torus :$ n = TorusS n

deriving instance Show (TorusS n)
deriving instance Eq (TorusS n)
deriving instance Ord (TorusS n)
instance ShowN Torus where getShow _ _ r = r
instance OrdN Torus where getOrd _ _ r = r


-- instance DeltaSet Torus where
--     dimension _ = HomogenousDim 2
--     face _ n i = caseN2 n
--                     (const Torus0)
--                     (\_ -> case i of
--                                                0 -> TorusInner
--                                                1 -> TorusInner
--                                                2 -> TorusBoundary
--                                                _ -> throw (FaceIndexOutOfBounds i))
--                     (\_ _ -> error "impossible")
-- 
-- 
--     simps _ n = caseN3 n
--                      [Torus0]
--                      [TorusInner, TorusBoundary]
--                      [Torus2]
--                      (const [])
-- 
-- 
--     supers a n _ = simps a (successorTo n)
-- 

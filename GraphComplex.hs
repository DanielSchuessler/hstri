{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module GraphComplex where

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
import ExampleComplexes

#include "macros.h"


data GraphComplex a b = GraphComplex (Gr a b) Dim
    deriving Show

data EdgyPath a b n where
    EP0 :: LNode a -> EdgyPath a b N0
    EPCons :: LNode a -> b -> EdgyPath a b n -> EdgyPath a b (S n) 

deriving instance (Eq a, Eq b) => Eq (EdgyPath a b n)
deriving instance (Ord a, Ord b) => Ord (EdgyPath a b n)

addLabel :: Graph gr => gr t b -> Node -> (Node, t)
addLabel g node = (node, fromJust (lab g node))

pathsOfLength :: (NaturalNumber n, Graph gr) => gr a b -> n -> Node -> [EdgyPath a b n] 
pathsOfLength g n start = caseN n
    [EP0 (addLabel g start)]
    (\n' -> do
        (_,node,elbl) <- out g start
        p <- pathsOfLength g n' node
        return (EPCons (addLabel g start) elbl p)) 

-- transitiveClosurePathsOfLength :: (NaturalNumber n, Graph gr) => gr a b -> n -> Node -> [EdgyPath a [b] n] 
-- transitiveClosurePathsOfLength g n start = caseN n
    

instance (Show a, Show b) => Show (EdgyPath a b n) where
    showsPrec _ x =
        showString "Path { " .
        go x .
        showString " }"
      where
        go :: forall n'. EdgyPath a b n' -> ShowS
        go (EP0 (_,l)) = shows l
        go (EPCons (_,l) l' p) = 
            shows l . 
            showString " >{" .
            shows l' .
            showString "}> " .
            go p
            



type instance GraphComplex a b :$ n = EdgyPath a b n


instance DeltaSet (GraphComplex a b) where
   dimension (GraphComplex _ d) = d
   simps (GraphComplex g _) n = pathsOfLength g n =<< nodes g


bary :: (OrdN a, DeltaSet a) => a -> GraphComplex (AnySimplex a) FaceIx
bary a = GraphComplex (faceGraph a) (dimension a) 

testm :: GraphComplex (AnySimplex Moebius) FaceIx
testm = bary Moebius


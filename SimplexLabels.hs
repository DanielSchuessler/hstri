{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module SimplexLabels where

import HomogenousTuples
import Data.Map as Map
import Element
import Control.Applicative
import Control.Arrow
import DeltaSet
import Nat

#include "macros.h"

type family SimplexLabel a n 

class SimplexLabels a n where
    simplbl :: a -> n -> a :$ n -> SimplexLabel a n



type instance SimplexLabel (DisjointUnion a b) n = SimplexLabel a n

instance (SimplexLabels a n, SimplexLabels b n, SimplexLabel a n ~ SimplexLabel b n) => 
    SimplexLabels (DisjointUnion a b) n where

    simplbl (DisjointUnion a b) n = simplbl a n ||| simplbl b n 



data WithSimplexLabels a n_lbl l = WithSimplexLabels a (a :$ n_lbl -> l)

instance Show a => Show (WithSimplexLabels a n_lbl l) where
    show (WithSimplexLabels a _) = show a


type instance SimplexLabel (WithSimplexLabels a n_lbl l) k = 
    IfEq k n_lbl l (SimplexLabel a k)

DERIVE_DELTA_SET(WithSimplexLabels a n_lbl l,(\(WithSimplexLabels a _) -> a))

instance (DecidableEq n new_n_lbl, 
          SimplexLabels (IfEq n new_n_lbl SimplexLabelsDummy a) n) =>

    SimplexLabels (WithSimplexLabels a new_n_lbl l) n where

    simplbl (WithSimplexLabels a f) n = 
        case getEq n (undefined :: new_n_lbl) of
             Equal -> f
             NotEqual -> simplbl a n

data SimplexLabelsDummy
instance SimplexLabels SimplexLabelsDummy n where
    simplbl = error "impossible"

                



addSimplexLabels :: n -> (a :$ n -> l) -> a -> WithSimplexLabels a n l
addSimplexLabels _ = flip WithSimplexLabels

mapSimplexLabels :: forall a n lnew. SimplexLabels a n => 
    n -> (SimplexLabel a n -> lnew) -> a -> WithSimplexLabels a n lnew
mapSimplexLabels n t a = addSimplexLabels n (t . simplbl a n) a


testLabels
  :: a
     -> WithSimplexLabels
          (WithSimplexLabels (WithSimplexLabels a N0 [Char]) N1 [Char])
          N0
          [Char]
testLabels = addSimplexLabels n0 f0' . addSimplexLabels n1 f1 . addSimplexLabels n0 f0
    where
        f0 _ = "f0"
        f1 _ = "f1"
        f0' _ = "f0'"

vertlbl :: SimplexLabels a N0 => a -> Vert a -> SimplexLabel a N0
vertlbl = flip simplbl n0





-- transformCoordsM :: SimplexLabels a => Proj4 -> a -> WithSimplexLabels a
-- transformCoordsM m = mapSimplexLabels (trim . (.* fromProjective m) . extendWith 1)

-- instance SimplexLabels (DS Vec3 s1 s2 s3) where
--     simplbl _ = id


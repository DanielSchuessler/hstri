{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, BangPatterns, NoMonomorphismRestriction, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, NamedFieldPuns, FlexibleContexts, TypeFamilies, OverlappingInstances, CPP #-} 
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-type-defaults #-} -- disabled warnings are because Printf-TH triggers them

-- | Utility functions for Patrick Bahr's /equivalence/ package
module Equivalence.Class(
    -- * Typeclasses
    IsEquivalenceClass(..),HasEquivalence(..), eqvRep, eqvEquivalents)   where

import Element


#define KEYCLASS(A) Ord(A)

class AsList cls => IsEquivalenceClass cls where
    canonicalRep :: cls -> Element cls
    ecMember :: Element cls -> cls -> Bool
    ecSize :: cls -> Int

class (Element cls ~ elt, IsEquivalenceClass cls) => 
    HasEquivalence t cls elt | cls -> elt, elt t -> cls where

    eqvClasses :: t -> [cls]
    -- | Throws an error if the element is not in the domain of the equivalence
    eqvClassOf :: t -> elt -> cls

eqvRep ::  HasEquivalence t cls elt => t -> elt -> elt
eqvRep e x = canonicalRep $! eqvClassOf e x 

eqvEquivalents ::  HasEquivalence t cls elt => t -> elt -> [elt]
eqvEquivalents e x = asList $! eqvClassOf e x


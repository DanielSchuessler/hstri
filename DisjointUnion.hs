{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module DisjointUnion(module Either1, DisjointUnionable(..), GDisjointUnionable, defaultDisjointUnion, CoDisjointUnionable(..), isRegardedAsSimplexByDisjointUnionDeriving) where

import Control.Monad
import Simplicial.DeltaSet3
import Either1
import GHC.Generics
import Language.Haskell.TH



class DisjointUnionable a b c | a b -> c where
    disjointUnion :: a -> b -> c


defaultDisjointUnion :: (Generic a, Generic b, Generic c, GDisjointUnionable (Rep a) (Rep b) (Rep c)) 
    => a -> b -> c
defaultDisjointUnion a b = to (gDisjointUnion (from a) (from b)) 

-- instance DisjointUnionable (a -> r) (a' -> r) (Either a a' -> r) where
--     disjointUnion = either

class CoDisjointUnionable a b c | a b -> c where
    coDjEither :: (a -> r) -> (b -> r) -> (c -> r)

instance (CoDisjointUnionable a b c) => DisjointUnionable (a -> r) (b -> r) (c -> r) where
    disjointUnion = coDjEither

-- instance CoDisjointUnionable (a n) (a' n) (Either1 a a' n) where
--     coDjEither = either1







class GDisjointUnionable a b c | a b -> c, c -> a b where
    gDisjointUnion :: a p -> b p -> c p

instance GDisjointUnionable U1 U1 U1 where
    gDisjointUnion _ _ = U1

instance (DisjointUnionable c1 c2 c) => GDisjointUnionable (K1 i1 c1) (K1 i2 c2) (K1 i c) where
    gDisjointUnion (K1 c1) (K1 c2) = K1 (disjointUnion c1 c2)

instance (GDisjointUnionable f1 f2 f) => GDisjointUnionable (M1 i1 c1 f1) (M1 i2 c2 f2) (M1 i c f) where
    gDisjointUnion (M1 f1) (M1 f2) = M1 (gDisjointUnion f1 f2)

instance (GDisjointUnionable f1 f2 f) => GDisjointUnionable (Rec1 f1) (Rec1 f2) (Rec1 f) where
    gDisjointUnion (Rec1 f1) (Rec1 f2) = Rec1 (gDisjointUnion f1 f2)

instance (GDisjointUnionable f1 f2 f, GDisjointUnionable g1 g2 g) =>
    GDisjointUnionable (f1 :*: g1) (f2 :*: g2) (f :*: g) where

    gDisjointUnion (f1 :*: g1) (f2 :*: g2) = gDisjointUnion f1 f2 :*: gDisjointUnion g1 g2 




-- data DJ a b = DJ { dj_fst :: a, dj_snd :: b }
--     deriving(Show)



instance (CoDisjointUnionable v1 v2 v, CoDisjointUnionable e1 e2 e) =>
    CoDisjointUnionable (AnySimplex1 v1 e1) (AnySimplex1 v2 e2) (AnySimplex1 v e)

    where

        coDjEither k1 k2 = 
            foldAnySimplex1
                (coDjEither (k1 . vertToAnySimplex1) (k2 . vertToAnySimplex1))
                (coDjEither (k1 . edToAnySimplex1) (k2 . edToAnySimplex1))


instance (CoDisjointUnionable v1 v2 v, CoDisjointUnionable e1 e2 e,
            CoDisjointUnionable t1 t2 t) =>
    CoDisjointUnionable (AnySimplex2 v1 e1 t1) (AnySimplex2 v2 e2 t2) (AnySimplex2 v e t)

    where

        coDjEither k1 k2 = 
            foldAnySimplex2'
                (coDjEither (k1 . anySimplex1To2) (k2 . anySimplex1To2))
                (coDjEither (k1 . triToAnySimplex2) (k2 . triToAnySimplex2))



isRegardedAsSimplexByDisjointUnionDeriving :: TypeQ -> DecsQ
isRegardedAsSimplexByDisjointUnionDeriving t = 
    let
        a = varT (mkName "a")
    in
        sequence
        [instanceD 
            (cxt [])
            (conT ''CoDisjointUnionable `appT` a `appT` t `appT` [t|Either $(a) $(t)|]) 
            [valD (varP 'coDjEither) (normalB (varE 'either)) []]
            ]



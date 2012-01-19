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

data DisjointUnion c inj1 inj2 djEither = 
    DisjointUnion {
        djObject :: c,
        djInjection1 :: inj1, 
        djInjection2 :: inj2,
        djEither :: djEither
    }

class DisjointUnionable a b c inj1 inj2 djEither | a b -> c inj1 inj2 djEither where
    disjointUnionWithInjs :: a -> b -> DisjointUnion c inj1 inj2 djEither

disjointUnion :: DisjointUnionable a b c inj1 inj2 djEither => a -> b -> c
disjointUnion = fmap djObject . disjointUnionWithInjs


defaultDisjointUnion a b = 
    case gDisjointUnion (from a) (from b) of

         DisjointUnion o inj1 inj2 ei -> 
            DisjointUnion (to o) (to inj1) (to inj2) (to ei) 

-- instance DisjointUnionable (a -> r) (a' -> r) (Either a a' -> r) where
--     disjointUnion = either

class CoDisjointUnionable a b c | a b -> c where
    coDjEither :: (a -> r) -> (b -> r) -> (c -> r)

instance (CoDisjointUnionable a b c) => DisjointUnionable (a -> r) (b -> r) (c -> r) where
    disjointUnion = coDjEither

-- instance CoDisjointUnionable (a n) (a' n) (Either1 a a' n) where
--     coDjEither = either1





class GDisjointUnionable a b c inj1 inj2 ei | a b -> c inj1 inj2 ei, c -> a b where
    gDisjointUnion :: a p -> b p -> DisjointUnion (c p) (inj1 p) (inj2 p) (ei p)

instance GDisjointUnionable U1 U1 U1 U1 U1 U1 where
    gDisjointUnion _ _ = DisjointUnion U1 U1 U1 U1

instance (DisjointUnionable c1 c2 c inj1 inj2 ei) => 
    GDisjointUnionable (K1 i1 c1) (K1 i2 c2) (K1 i c) (K1 i_ inj1) (K1 i__ inj2) (K i___ ei)  where

    gDisjointUnion (K1 c1) (K1 c2) = 
        case disjointUnionWithInjs c1 c2 of
             DisjointUnion o inj1 inj2 ei -> DisjointUnion (K1 o) (K1 inj1) (K1 inj2) (K1 ei)

instance (GDisjointUnionable f1 f2 f inj1 inj2 ei) => 
    GDisjointUnionable (M1 i1 c1 f1) (M1 i2 c2 f2) 
        (M1 i c f) (M1 i_ c_ inj1) (M1 i__ c__ inj2) (M1 i___ c___ ei) where

    gDisjointUnion (M1 c1) (M1 c2) = 
        case gDisjointUnion c1 c2 of
             DisjointUnion o inj1 inj2 ei -> DisjointUnion (M1 o) (M1 inj1) (M1 inj2) (M1 ei)

instance (GDisjointUnionable c1 c2 c inj1 inj2 ei) => 
    GDisjointUnionable (Rec1 c1) (Rec1 c2) (Rec1 c) (Rec1 inj1) (Rec1 inj2) (Rec1 ei) where

    gDisjointUnion (Rec1 c1) (Rec1 c2) = 
        case gDisjointUnion c1 c2 of
             DisjointUnion o inj1 inj2 ei -> 
                DisjointUnion (Rec1 o) (Rec1 inj1) (Rec1 inj2) (Rec1 ei)

instance (GDisjointUnionable f1 f2 f finj1 finj2 fei, GDisjointUnionable g1 g2 g ginj1 ginj2 gei) =>

    GDisjointUnionable (f1 :*: g1) (f2 :*: g2) 
        (f :*: g) (finj1 :*: ginj1) (finj2 :*: ginj2) (fei :*: gei) where

    gDisjointUnion (f1 :*: g1) (f2 :*: g2) = 
    
        case (gDisjointUnion f1 f2, gDisjointUnion g1 g2) of
             ( DisjointUnion o inj1 inj2 ei
              ,DisjointUnion o' inj1' inj2' ei') ->

                 DisjointUnion (o:*:o') (inj1:*:inj1') (inj2:*:inj2') (ei :*: ei')




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



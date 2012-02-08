{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
module DisjointUnion(module Either1, module Data.SumType, DisjointUnion(..), DisjointUnionable(..), disjointUnion, GDisjointUnionable, defaultDisjointUnion, CoDisjointUnionable(..), isRegardedAsSimplexByDisjointUnionDeriving, djZapUnits, DJSimp(..),DJSimps(..),DJSimpsH(..)) where

import Control.Monad
import Simplicial.DeltaSet3
import Either1
import GHC.Generics
import Language.Haskell.TH
import ZapUnits
import Data.SumType
import ShortShow
import PrettyUtil
import Language.Haskell.TH.Lift

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


defaultDisjointUnion
  :: (Generic a1,
      Generic a2,
      Generic c,
      GDisjointUnionable (Rep a1) (Rep a2) (Rep c) inj1 inj2 djEither) =>
     a1 -> a2 -> DisjointUnion c inj1 inj2 djEither
defaultDisjointUnion a b = 
    case gDisjointUnion (from a) (from b) of

         DisjointUnion o inj1 inj2 ei -> 
            DisjointUnion (to o) inj1 inj2 ei

-- instance DisjointUnionable (a -> r) (a' -> r) (Either a a' -> r) where
--     disjointUnion = either

class CoDisjointUnionable a b c | a b -> c where
    coDjEither :: (a -> r) -> (b -> r) -> (c -> r)

instance (CoDisjointUnionable a b c) => 
    DisjointUnionable (a -> r) (b -> r) (c -> r) () () () where

    disjointUnionWithInjs fa fb = DisjointUnion (coDjEither fa fb) () () ()

-- instance CoDisjointUnionable (a n) (a' n) (Either1 a a' n) where
--     coDjEither = either1





class GDisjointUnionable a b c inj1 inj2 ei | a b -> c inj1 inj2 ei, c -> a b where
    gDisjointUnion :: a p -> b p -> DisjointUnion (c p) inj1 inj2 ei

instance GDisjointUnionable U1 U1 U1 () () () where
    gDisjointUnion _ _ = DisjointUnion U1 () () ()

instance (DisjointUnionable c1 c2 c inj1 inj2 ei) => 
    GDisjointUnionable (K1 i1 c1) (K1 i2 c2) (K1 i c) inj1 inj2 ei  where

    gDisjointUnion (K1 c1) (K1 c2) = 
        case disjointUnionWithInjs c1 c2 of
             DisjointUnion o inj1 inj2 ei -> DisjointUnion (K1 o) inj1 inj2 ei

instance (GDisjointUnionable f1 f2 f inj1 inj2 ei) => 
    GDisjointUnionable (M1 i1 c1 f1) (M1 i2 c2 f2) 
        (M1 i c f) inj1 inj2 ei where

    gDisjointUnion (M1 c1) (M1 c2) = 
        case gDisjointUnion c1 c2 of
             DisjointUnion o inj1 inj2 ei -> DisjointUnion (M1 o) inj1 inj2 ei

instance (GDisjointUnionable c1 c2 c inj1 inj2 ei) => 
    GDisjointUnionable (Rec1 c1) (Rec1 c2) (Rec1 c) inj1 inj2 ei where

    gDisjointUnion (Rec1 c1) (Rec1 c2) = 
        case gDisjointUnion c1 c2 of
             DisjointUnion o inj1 inj2 ei -> 
                DisjointUnion (Rec1 o) inj1 inj2 ei

instance 
    (GDisjointUnionable f1 f2 f finj1 finj2 fei, 
     GDisjointUnionable g1 g2 g ginj1 ginj2 gei) =>

    GDisjointUnionable (f1 :*: g1) (f2 :*: g2) 
        (f :*: g) (finj1, ginj1) (finj2, ginj2) (fei, gei) where

    gDisjointUnion (f1 :*: g1) (f2 :*: g2) = 
    
        case (gDisjointUnion f1 f2, gDisjointUnion g1 g2) of
             ( DisjointUnion o inj1 inj2 ei
              ,DisjointUnion o' inj1' inj2' ei') ->

                 DisjointUnion (o:*:o') (inj1, inj1') (inj2, inj2') (ei, ei')




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






djZapUnits
  :: (ZapUnits a inj1, ZapUnits a1 inj2, ZapUnits a2 djEither) =>
     DisjointUnion c a a1 a2 -> DisjointUnion c inj1 inj2 djEither
djZapUnits (DisjointUnion o a b c) = DisjointUnion o (zapUnits a) (zapUnits b) (zapUnits c)







-- | Disjoint union simplex
newtype DJSimp dim a b = DJSimp (Either a b)
    deriving(Show,Eq,Ord,SubSumTy,SuperSumTy,Pretty)

type instance L (DJSimp dim a b) = a
type instance Data.SumType.R (DJSimp dim a b) = b

deriving instance ShortShow (Either a b) => ShortShow (DJSimp dim a b)

instance CoDisjointUnionable a (DJSimp dim b b') (DJSimp dim a (DJSimp dim b b'))

            where

                coDjEither = either'

isRegardedAsSimplexByDisjointUnionDeriving
  :: Name -> TypeQ -> Q [Dec]
isRegardedAsSimplexByDisjointUnionDeriving dim t = 
    let
        a = varT (mkName "a")
        go x y = instanceD 
            (cxt [])
            (conT ''CoDisjointUnionable `appT` x `appT` y `appT` [t|DJSimp $(conT dim) $(x) $(y)|]) 
            [valD (varP 'coDjEither) (normalB (varE 'either')) []]
          
    in
        sequence
        [ go a t
        ]
--         , go t a
--         , go t t]



-- | Mixed collection of simplices from the first and second summand complex
data DJSimps dim as bs = DJSimps { leftSimps :: as, rightSimps :: bs } 
    deriving Show

type instance Element (DJSimps dim as bs) = DJSimp dim (Element as) (Element bs) 

instance (AsList as, AsList bs) => AsList (DJSimps dim as bs) where
    asList (DJSimps as bs) = (map left' . asList) as ++ (map right' . asList) bs

type instance L (DJSimpsH dim a b) = a
type instance Data.SumType.R (DJSimpsH dim a b) = b

-- | Either a collection of simplices from the first summand complex, or from the second
newtype DJSimpsH dim as bs = DJSimpsH (Either as bs)
    deriving(SubSumTy,SuperSumTy,Show)

type instance Element (DJSimpsH dim as bs) = DJSimp dim (Element as) (Element bs) 

instance (AsList as, AsList bs) => AsList (DJSimpsH dim as bs) where
    asList = (map left' . asList) |||| (map right' . asList)

-- #define F(Vertices,Verts,Vert,vertices,Pair,map2)\
--     instance (Vertices a, Vertices b, Verts a ~ Pair (Vert a), Verts b ~ Pair (Vert b))\
--         => Vertices (DJSimp a b) where {\
--     \
--         type Verts (DJSimp a b) = Pair (DJSimp (Vert a) (Vert b));\
--     \
--         vertices = (map2 left' . vertices) |||| (map2 right' . vertices)};
-- 
-- 
-- #define G(X,Y)\
-- F(Vertices,Verts,Vert,vertices,X,Y)\
-- F(Edges,Eds,Ed,edges,X,Y)\
-- F(Triangles,Tris,Tri,triangles,X,Y)\
-- F(Tetrahedra,Tets,Tet,tetrahedra,X,Y)
-- 
-- G(Pair,map2)
-- G(Triple,map3)
-- G(Quadruple,map4)
-- 
-- #undef F



instance (EdgeLike a, EdgeLike b) => Vertices (DJSimp DIM1 a b) where
    type Verts (DJSimp DIM1 a b) = Pair (DJSimp DIM0 (Vert a) (Vert b))

    vertices =   (map2 left' . vertices) |||| 
                 (map2 right' . vertices)

instance (TriangleLike a, TriangleLike b) => Vertices (DJSimp DIM2 a b) where
    type Verts (DJSimp DIM2 a b) = Triple (DJSimp DIM0 (Vert a) (Vert b))

    vertices =   (map3 left' . vertices) |||| 
                 (map3 right' . vertices)

instance (TriangleLike a, TriangleLike b) => Edges (DJSimp DIM2 a b) where
    type Eds (DJSimp DIM2 a b) = Triple (DJSimp DIM1 (Ed a) (Ed b))

    edges = (map3 left' . edges) |||| 
            (map3 right' . edges)

instance (SatisfiesSimplicialIdentities2 a, SatisfiesSimplicialIdentities2 b) => SatisfiesSimplicialIdentities2 (DJSimp DIM2 a b)

instance (TetrahedronLike a, TetrahedronLike b) => Vertices (DJSimp DIM3 a b) where
    type Verts (DJSimp DIM3 a b) = Quadruple (DJSimp DIM0 (Vert a) (Vert b))

    vertices =   (map4 left' . vertices) |||| 
                 (map4 right' . vertices)

instance (TetrahedronLike a, TetrahedronLike b) => Edges (DJSimp DIM3 a b) where
    type Eds (DJSimp DIM3 a b) = Sextuple (DJSimp DIM1 (Ed a) (Ed b))

    edges = (map6 left' . edges) |||| 
            (map6 right' . edges)

instance (TetrahedronLike a, TetrahedronLike b) => Triangles (DJSimp DIM3 a b) where
    type Tris (DJSimp DIM3 a b) = Quadruple (DJSimp DIM2 (Tri a) (Tri b))

    triangles = (map4 left' . triangles) |||| 
                (map4 right' . triangles)

instance (SatisfiesSimplicialIdentities3 a, SatisfiesSimplicialIdentities3 b) => SatisfiesSimplicialIdentities3 (DJSimp DIM3 a b)


deriveLiftMany [''DJSimp]

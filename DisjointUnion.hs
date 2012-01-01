{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module DisjointUnion(module Either1, DisjointUnionable(..), GDisjointUnionable, defaultDisjointUnion) where

import Control.Applicative
import Control.Monad
import GraphUtil
import Simplicial.DeltaSet
import Numbering
import Simplicial.AnySimplex
import Either1
import GHC.Generics




class DisjointUnionable a b c | a b -> c, c -> a b where
    disjointUnion :: a -> b -> c


defaultDisjointUnion :: (Generic a, Generic b, Generic c, GDisjointUnionable (Rep a) (Rep b) (Rep c)) 
    => a -> b -> c
defaultDisjointUnion a b = to (gDisjointUnion (from a) (from b)) 

-- instance DisjointUnionable (a -> r) (a' -> r) (Either a a' -> r) where
--     disjointUnion = either

instance DisjointUnionable (a n -> r) (a' n -> r) (Either1 a a' n -> r) where
    disjointUnion = either1

instance DisjointUnionable (AnySimplex a -> r) (AnySimplex a' -> r) (AnySimplex (Either1 a a') -> r) where
    disjointUnion f f' (AnySimplex x) = either1 (f . AnySimplex) (f' . AnySimplex) x

instance DisjointUnionable (DeltaSet a) (DeltaSet b) (DeltaSet (Either1 a b)) where

    disjointUnion a b = DeltaSet face' simps'_ supers' dimension' faceGraph' nodeMap'
        (disjointUnionIndexing (simpsIndexing a) (simpsIndexing b) Left1 Right1)
                            
     where

        dimension' =
                    case (dimension a, dimension b) of
                        (hda@(HomogenousDim da), (HomogenousDim db)) | da == db -> hda
                        (da, db) -> InhomogenousDim (max (dimMax da) (dimMax db))


        face' :: FaceFunction (Either1 a b)
        face' i = 
                    face a i `bimap1` face b i

        simps'_ :: SimpsFunction (Either1 a b)
        simps'_ n = 
                    (Left1 <$> simps a n) ++ (Right1 <$> simps b n)

        
        supers' :: SuperFunction (Either1 a b)
        supers' = 
                    (fmap Left1  . supers a) `either1` 
                    (fmap Right1 . supers b)

        faceGraph' :: FaceGraph (Either1 a b)
        (faceGraph',nodeEmbedding) = 
            disjointUnionGraphs 
                        (\(AnySimplex x) -> AnySimplex (Left1 x))
                        id 
                        (\(AnySimplex x) -> AnySimplex (Right1 x))
                        id 
                        (faceGraph a) 
                        (faceGraph b)


        nodeMap' (AnySimplex x) =
            case x of
                 Left1 x' -> nodeMap a (AnySimplex x') 
                 Right1 x' -> nodeEmbedding (nodeMap b (AnySimplex x'))



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



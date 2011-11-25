{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module DisjointUnion(module Either1, DisjointUnionable(..)) where

import Control.Applicative
import Control.Monad
import GraphUtil
import Simplicial.DeltaSet
import Indexing
import Simplicial.AnySimplex
import Either1




class DisjointUnionable a b where
    type DisjointUnion a b
    disjointUnion :: a -> b -> DisjointUnion a b


instance DisjointUnionable (DeltaSet a) (DeltaSet b) where
    type (DisjointUnion (DeltaSet a) (DeltaSet b)) = DeltaSet (Either1 a b)

    disjointUnion a b = DeltaSet face' simps' supers' dimension' faceGraph' nodeMap'
        (disjointUnionIndexing (simpsIndexing a) (simpsIndexing b) Left1 Right1)
                            
     where

        dimension' =
                    case (dimension a, dimension b) of
                        (hda@(HomogenousDim da), (HomogenousDim db)) | da == db -> hda
                        (da, db) -> InhomogenousDim (max (dimMax da) (dimMax db))


        face' :: FaceFunction (Either1 a b)
        face' i = 
                    face a i `bimap1` face b i

        simps' :: SimpsFunction (Either1 a b)
        simps' = 
                    (Left1 <$> simps a) ++ (Right1 <$> simps b)

        
        supers' :: SuperFunction (Either1 a b)
        supers' = 
                    (fmap Left1  . supers a) `either1` 
                    (fmap Right1 . supers b)

        faceGraph' :: FaceGraph (Either1 a b)
        faceGraph' = disjointUnionGraphs 
                        (\(AnySimplex x) -> AnySimplex (Left1 x))
                        id 
                        (\(AnySimplex x) -> AnySimplex (Right1 x))
                        id 
                        (faceGraph a) 
                        (faceGraph b)


        nodeMap' (AnySimplex x) =
            case x of
                 Left1 x' -> nodeMap a (AnySimplex x') 
                 Right1 x' -> nodeMap b (AnySimplex x') 

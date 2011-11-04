{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module DisjointUnion where

import Control.Applicative
import Control.Arrow
import Control.Monad
import GraphUtil
import DeltaSet
import TypeLevel.TF

#include "macros.h"


data EitherSequence a b

type instance (EitherSequence a b) :$ n = Either (a :$ n) (b :$ n)

class DisjointUnionable a b where
    type DisjointUnion a b
    disjointUnion :: a -> b -> DisjointUnion a b


instance DisjointUnionable (DeltaSet a) (DeltaSet b) where
    type (DisjointUnion (DeltaSet a) (DeltaSet b)) = DeltaSet (EitherSequence a b)

    disjointUnion a b = DeltaSet face' simps' supers' dimension' faceGraph' nodeMap'
     where

        dimension' =
                    case (dimension a, dimension b) of
                        (hda@(HomogenousDim da), (HomogenousDim db)) | da == db -> hda
                        (da, db) -> InhomogenousDim (max (dimMax da) (dimMax db))


        face' :: FaceFunction (EitherSequence a b)
        face' n i = 
                    face a n i +++ face b n i

        simps' :: SimpsFunction (EitherSequence a b)
        simps' n = 
                    (Left <$> simps a n) ++ (Right <$> simps b n)

        
        supers' :: SuperFunction (EitherSequence a b)
        supers' n = 
                    (fmap Left  . supers a n) ||| 
                    (fmap Right . supers b n)

        faceGraph' :: FaceGraph (EitherSequence a b)
        faceGraph' = disjointUnionGraphs 
                        (\(AnySimplex n x) -> AnySimplex n (Left x))
                        id 
                        (\(AnySimplex n x) -> AnySimplex n (Right x))
                        id 
                        (faceGraph a) 
                        (faceGraph b)


        nodeMap' (AnySimplex n x) =
            case x of
                 Left x' -> nodeMap a (AnySimplex n x') 
                 Right x' -> nodeMap b (AnySimplex n x') 

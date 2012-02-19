{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module QuadHalf where

import FaceClasses
import Data.SumType
import ShortShow
import PrettyUtil
import HomogenousTuples
import TIndex
import Control.Arrow
import Control.Exception
import Test.QuickCheck

data WhichHalf = Half102 | Half023
    deriving (Show,Eq,Ord)

instance ShortShow WhichHalf where
    shortShow = drop 4 . show

instance Pretty WhichHalf where
    pretty = text . show

instance Arbitrary WhichHalf where
    arbitrary = elements [Half102,Half023]

newtype QuadHalf q = QH (WhichHalf, q) 
    deriving(Show,ShortShow,Eq,Ord,Pretty,Functor,Arbitrary)


newtype AdjoinQuadDiags e q = 
        AdjoinQuadDiags (Either e q)
    deriving (Show,ShortShow,Eq,Ord,SubSumTy,SuperSumTy)

type instance L (AdjoinQuadDiags e q) = e
type instance R (AdjoinQuadDiags e q) = q

instance (Pretty e, Pretty q) => Pretty (AdjoinQuadDiags e q) where
    prettyPrec prec = either' (prettyPrec prec) (\x -> prettyPrecApp prec "diag" [x])


newtype TriOrQuadHalf t q = 
        TriOrQuadHalf (Either t (QuadHalf q))
    deriving (Show,ShortShow,Eq,Ord,SubSumTy,SuperSumTy)

type instance L (TriOrQuadHalf t q) = t
type instance R (TriOrQuadHalf t q) = QuadHalf q

instance (Pretty t, Pretty q) => Pretty (TriOrQuadHalf t q) where
    prettyPrec prec = either' (prettyPrec prec) (prettyPrec prec)

qh_quad (QH (_,q)) = q

qh_which (QH (w,_)) = w

instance (Vertices q , Verts q ~ Quadruple (Vert q)) 
    => Vertices (QuadHalf q) where

    type Verts (QuadHalf q) = Triple (Vert q)
    vertices qh = 
                    -- see comment below ("brittle hack")
                    case qh_which qh of
                       Half102 -> (v1,v0,v2)
                       Half023 -> (v0,v2,v3)
        where
            (v0,v1,v2,v3) = vertices (qh_quad qh)




instance (Edges q , Eds q ~ Quadruple (Ed q)) 
    => Edges (QuadHalf q) where

    type Eds (QuadHalf q) = Triple (AdjoinQuadDiags (Ed q) q)
-- instance Edges CNAO

    edges qh = case qh_which qh of
                    -- Brittle hack depending on the fact that the Vertices and Edges instances for
                    -- q behave in a certain way, which q=NormalQuad happens to satisfy.
                    -- Namely, 
                    -- 1. If (v0,v1,v2,v3) = vertices q, then v1 < v0 < v2 < v3 ("v1 < v0" not a typo)
                    -- 2. If (v0,v1,v2,v3) = vertices q, then (v0v3,v0v1,v1v2,v2v3) = edges q
                       Half102 -> (left' e01, left' e12, right' q)
                       Half023 -> (right' q, left' e03,left' e23)
        where
            (e03,e01,e12,e23) = edges q
            q = (qh_quad qh)

instance (e ~ Ed q, Vert q ~ Vert e, EdgeLike e, Vertices q , Verts q ~ Quadruple (Vert q)) 
    => Vertices (AdjoinQuadDiags e q) where

    type Verts (AdjoinQuadDiags e q) = Pair (Vert q)

    vertices = either' vertices 
        (\q -> 
            let
                (v0,_,v2,_) = vertices q
            in
                (v0,v2))

instance (Vert t ~ Vert q, TriangleLike t, Vertices q , Verts q ~ Quadruple (Vert q)) 
    => Vertices (TriOrQuadHalf t q) where

    type Verts (TriOrQuadHalf t q) = Triple (Vert t)
    vertices = either' vertices vertices

instance (Ed t ~ Ed q, TriangleLike t, Edges q , Eds q ~ Quadruple (Ed q)) 
    => Edges (TriOrQuadHalf t q) where

    type Eds (TriOrQuadHalf t q) = Triple (AdjoinQuadDiags (Ed q) q)
    edges = either' (map3 left' . edges) edges
                

instance MapTIndices iq => MapTIndices (QuadHalf iq) where
    mapTIndices f (QH x) = QH (second (mapTIndices f) x)

instance HasTIndex iq q => HasTIndex (QuadHalf iq) (QuadHalf q) where
    viewI (QH (w,viewI -> I i q)) = I i (QH (w,q))
    i ./ QH (w,q) = QH (w,i./q)


instance (MapTIndices it, MapTIndices iq) => MapTIndices (TriOrQuadHalf it iq) where
    mapTIndices = mapTIndices_sum

instance (HasTIndex it t, HasTIndex iq q) => HasTIndex (TriOrQuadHalf it iq) (TriOrQuadHalf t q) where
    viewI = viewI_sum 
    (./) = addTIndex_sum
    

instance (MapTIndices it, MapTIndices iq) => MapTIndices (AdjoinQuadDiags it iq) where
    mapTIndices = mapTIndices_sum

instance (HasTIndex it t, HasTIndex iq q) => HasTIndex (AdjoinQuadDiags it iq) (AdjoinQuadDiags t q) where
    viewI = viewI_sum 
    (./) = addTIndex_sum



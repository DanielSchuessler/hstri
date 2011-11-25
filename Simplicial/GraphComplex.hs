{-# LANGUAGE TypeOperators, GADTs, FlexibleInstances, TemplateHaskell, TypeFamilies, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module Simplicial.GraphComplex where

import Control.Exception
import Control.Monad
import Data.Graph.Inductive
import Data.List as List
import Data.Maybe
import Data.Semigroup
import Data.Vect.Double.Base hiding(dim)
import FaceIx
import GraphUtil
import Simplicial.AnySimplex
import Simplicial.DeltaSet
import Simplicial.StrictlyIncreasingMap

-- | Node-and-edge-labelled paths of edge-length @n@ (thus, at least one node)
data EPath a b n where
    EP0 :: LNode a -> EPath a b N0
    EPCons :: EPath a b n -> b -> LNode a -> EPath a b (S n) 


epN :: EPath a b n -> Int
epN (EP0 _) = 0
epN (EPCons p _ _) = 1+epN p

instance (Show a, Show b) => Show (EPath a b n) where
    showsPrec _ x =
        showString "(" .
        shows (epN x) .
        showString ") { " .
        go x .
        showString " }"
      where
        go :: forall n'. EPath a b n' -> ShowS
        go (EP0 (_,l)) = shows l
        go (EPCons p l' (_,l)) = 
            go p .
            showString " -{" .
            shows l' .
            showString "}-> " .
            shows l 

epLast :: EPath a b n -> (LNode a)
epLast (EP0 x) = x
epLast (EPCons _ _ x) = x

epInit :: EPath a b (S n) -> EPath a b n
epInit (EPCons p _ _) = p

mapEPath :: (a -> a') -> (b -> b') -> EPath a b n -> EPath a' b' n
mapEPath fa fb p = case p of 
                        EP0 (n,a) -> EP0 (n,fa a)
                        EPCons p' b (n,a) -> EPCons (mapEPath fa fb p') (fb b) (n,fa a)


deriving instance (Eq a, Eq b) => Eq (EPath a b n)
deriving instance (Ord a, Ord b) => Ord (EPath a b n)

instance (Ord a, Ord b) => OrdN (EPath a b) where getOrd _ r = r
instance (Show a, Show b) => ShowN (EPath a b) where getShow _ r = r


addLabel :: Graph gr => gr t b -> Node -> (Node, t)
addLabel g node = (node, fromJust (lab g node))

gPathsOfLength :: forall n gr a b c. (Nat n, Graph gr) => 
    (forall k. Nat k => EPath a c k -> [(c,Node)]) -> 
    gr a b -> Node -> [EPath a c n] 
gPathsOfLength chooserFun g start = caseNat (undefined :: n)
    [EP0 (addLabel g start)]
    (\_ -> do
        p <- gPathsOfLength chooserFun g start
        (elbl,node) <- chooserFun p
        return (EPCons p elbl (addLabel g node))) 

pathsOfLength :: forall gr a b n. (Graph gr, Nat n) => gr a b -> Node -> [EPath a b n]
pathsOfLength g x = gPathsOfLength (fmap f . out g . unlabel . epLast) g x
                                     where
        f :: LEdge b -> (b,Node)
        f (_,node,elbl) = (elbl,node)

-- data SkipPath a b n where
--     SP0 :: a -> SkipPath a b N0
--     SPCons :: SkipPath a b n -> EPath a b 

-- | Edge labels and nodes skipped
data Skipped b = Sk0 b | SkCons (Skipped b) Node b
    deriving(Eq,Ord)

instance Show b => Show (Skipped b) where
    showsPrec prec (Sk0 b) = showsPrec prec b 
    showsPrec _ s_ = showString "Skip {" . go s_ . showString "}"
        where
            go (Sk0 b) = showString "--{" . showsPrec 0 b . showString "}-->"
            go (SkCons s n b) = 
                                    go s .
                                    showChar ' ' .
                                    shows n .
                                    showChar ' ' .
                                    go (Sk0 b) 

catSkipped :: Skipped b -> Node -> Skipped b -> Skipped b
catSkipped s no (Sk0 b) = SkCons s no b
catSkipped s no (SkCons s' no' b) = SkCons (catSkipped s no s') no' b

skippingPathsOfLength :: forall gr a b c n. 
    (Graph gr, Nat n, Semigroup c) => ((a,b,a) -> c) -> gr a b -> Node -> [EPath a c n]

skippingPathsOfLength emb g = gPathsOfLength (chooserFun . unlabel . epLast) g
    where
        chooserFun :: Node -> [(c, Node)]
        chooserFun node = do
            (_,nod',elbl) <- out g node 
            let lnod' = addLabel g nod'
            go (emb (fromJust (lab g node),elbl,nodeLabel lnod')) lnod'

        go :: c -> LNode a -> [(c, Node)]
        go acc lnod' = do
            let nod' = unlabel lnod'
            return (acc,nod')
                ++
                    (do
                        (_,nod'',elbl') <- out g nod' 
                        let lnod'' = addLabel g nod''
                        go (acc <> emb (nodeLabel lnod', elbl', nodeLabel lnod'')) 
                            lnod''
                    ) 






-- transitiveClosurePathsOfLength :: (Nat n, Graph gr) => gr a b -> n -> Node -> [EPath a [b] n] 
-- transitiveClosurePathsOfLength g n start = caseN n
    
graphComplexFace :: forall a c n. Semigroup c => FaceIx -> EPath a c (S n) -> EPath a c n
graphComplexFace 0 p = epInit p
graphComplexFace i (EPCons p sk right_) =
    case p of
         EP0 _ ->
            case i of
                1 -> EP0 right_
                _ -> throw (FaceIndexOutOfBounds i)

         EPCons _ sk' _ -> 

            EPCons 
                (graphComplexFace (i-1) p) 
                (case i of
                      1 -> sk' <> sk 
                      _ -> sk)
                right_


--         -- | Deletes the i-th node, starting from the right. If i=0, the dangling edge label is returned in the second component
--         go :: forall n. Nat n => 
--             n -> 
--             FaceIx ->
--             EPath a (Skipped b) (S n) ->
--             EPath a (Skipped b) n
-- 
--         go n 0 (EPCons p sk right_) =


        




-- | SkipPath (edge-labelled by the edges and nodes that were skipped)
type SPath a b = EPath a (Skipped b)

nmapSPath :: (a -> a') -> SPath a b n -> SPath a' b n
nmapSPath fa = mapEPath fa id

graphComplex :: (Ord a, Ord c, Semigroup c) => Gr a b -> Dim -> ((a,b,a) -> c) -> DeltaSet (EPath a c)
graphComplex g dim emb = 
    mkDeltaSet  
        graphComplexFace
        (concatMap (\nod -> skippingPathsOfLength emb g nod) (nodes g))
        dim

class Subdivisible a where
    type BarycentricSubdivision a 

    bary :: a -> BarycentricSubdivision a



type BCSFace a = EPath (AnySimplex a) StrictlyIncreasingMap 



instance OrdN a => Subdivisible (DeltaSet a) where
    type BarycentricSubdivision (DeltaSet a) = DeltaSet (BCSFace a)
    bary a = graphComplex 
                    (faceGraph a) (dimension a) 
                    (\(_,faceIx,s) -> cofaceMap (anySimplex_dim s) faceIx)  

-- foldrEPath :: (LNode a -> b -> r -> r) -> (LNode a -> r) -> EPath a b n -> r 
-- foldrEPath c z (EP0 x) = z x
-- foldrEPath c z (EPCons p b x) = go 0


-- Note that this is actually like a list left fold structurally.
-- Also returns @n@ as a value.
foldrEPath :: forall v b n r. Vector v => (LNode v -> r) -> (LNode v -> r -> r) -> EPath v b n -> (Int,r)
foldrEPath z _ (EP0 x) = (0, z x)
foldrEPath z c (EPCons p _ x) = go 1 (z x) p
    where
        go :: forall n'. Int -> r -> EPath v b n' -> (Int,r) 
        go i acc (EP0 y) = (i, c y acc)
        go i acc (EPCons p' _ y) = go (i+1) (c y acc) p' 

barycenter :: forall v b n. Vector v => EPath v b n -> v
barycenter p = case foldrEPath nodeLabel ((&+) . nodeLabel) p of
                  (i,v) -> v &* recip (fromIntegral (i+1))


-- 
-- testm :: GraphComplex (AnySimplex Moebius) FaceIx
-- testm = bary Moebius
-- 

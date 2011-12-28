{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, FlexibleContexts, ViewPatterns, MultiParamTypeClasses, TypeFamilies, FunctionalDependencies #-}
{-# OPTIONS -Wall #-}
module OrderableFace(
    module Group,
    module FaceClasses,    
    OrderableFace(..),
    defaultRightActionForOrderedFace,
    defaultVerticesForOrderedFace,
    forgetVertexOrder,
    getVertexOrder,
    toOrderedFace,
    defaultPackOrderedFaceI,
    defaultUnpackOrderedFaceI,

    polyprop_OrderableFace
    
    ) where

import Group
import FaceClasses
import Data.Monoid
import QuickCheckUtil
import Test.QuickCheck
import Data.Proxy
import TIndex
import Control.Arrow


-- | Parameters: 
--
-- * @t@: Face with unordered vertices (also represents faces with vertices ordered canonically)  
--
-- * @ot@: Face with ordered vertices                                                              
--
-- CONTRACT:
--
-- * @vertices (packOrderedFace x g) == vertices x *. g@
--
-- * @packOrderedFace x (g1 .*. g2) == (packOrderedFace x g1) *. g2@
--
-- * @id == uncurry packOrderedFace . unpackOrderedFace@
--
-- * 'packOrderedFace' and 'unpackOrderedFace' must be inverses of each other (in other words, @ot@ is isomorphic to @(VertexSymGroup t, t)@, but I leave open the possibility to use a more efficient representation in the future)
class ( Group (VertexSymGroup t)
      , RightAction (VertexSymGroup t) (VertexTuple t)
      , RightAction (VertexSymGroup t) ot
      , Vertices t  (VertexTuple t)
      , Vertices ot (VertexTuple t) 
      )
      
      => OrderableFace 
            t  
            ot 
            
            | t -> ot, ot -> t where

    type VertexSymGroup t
    type VertexTuple t
    unpackOrderedFace :: ot -> (t,VertexSymGroup t)
    packOrderedFace ::  t -> (VertexSymGroup t) -> ot


defaultVerticesForOrderedFace :: OrderableFace t ot => ot -> VertexTuple t
defaultVerticesForOrderedFace (unpackOrderedFace -> (x,g)) = vertices x *. g

defaultRightActionForOrderedFace :: OrderableFace t ot => ot -> VertexSymGroup t -> ot
defaultRightActionForOrderedFace (unpackOrderedFace -> (x,g1)) g2 = packOrderedFace x (g1 .*. g2)

polyprop_OrderableFace :: forall t ot. 
    (Show t, Show ot, Show (VertexSymGroup t), Show (VertexTuple t),
     OrderableFace t ot,
     Arbitrary t, Arbitrary ot, Arbitrary (VertexSymGroup t),
     Eq (VertexTuple t), Eq ot) =>
    Proxy t -> Property
polyprop_OrderableFace _ =
    p1 .&. p2 .&. p3
 where
    p1 (x :: t) g =
        vertices (packOrderedFace x g) .=. vertices x *. g

    p2 (x :: t) g1 g2 =
        packOrderedFace x (g1 .*. g2) .=. packOrderedFace x g1 *. g2
            
    p3 (ox :: ot) =
        ox .=. uncurry packOrderedFace (unpackOrderedFace ox)

forgetVertexOrder ::  OrderableFace t ot => ot -> t
forgetVertexOrder = fst . unpackOrderedFace

getVertexOrder :: OrderableFace t ot => ot -> VertexSymGroup t
getVertexOrder = snd . unpackOrderedFace

-- | Equivalent to 'packOrderedFace mempty'
toOrderedFace :: OrderableFace t ot => t -> ot
toOrderedFace = flip packOrderedFace mempty

defaultUnpackOrderedFaceI
  :: (HasTIndex ia a, HasTIndex ib b, OrderableFace b a) =>
     ia -> (ib, VertexSymGroup b)
defaultUnpackOrderedFaceI = traverseI first unpackOrderedFace

defaultPackOrderedFaceI
  :: (HasTIndex ia a, HasTIndex ib b, OrderableFace a b) =>
     ia -> VertexSymGroup a -> ib
defaultPackOrderedFaceI = traverseI fmap packOrderedFace


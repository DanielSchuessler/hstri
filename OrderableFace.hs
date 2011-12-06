{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ViewPatterns, MultiParamTypeClasses, TypeFamilies, FunctionalDependencies #-}
{-# OPTIONS -Wall #-}
module OrderableFace(
    module Group,
    module FaceClasses,    
    OrderableFace(..),
    defaultLeftActionForOrderedFace,
    defaultVerticesForOrderedFace,
    forgetVertexOrder,
    toOrderedFace
    
    ) where

import Group
import FaceClasses
import Data.Monoid


-- | Parameters: 
--
-- * @t@: Face with unordered vertices (also represents faces with vertices ordered canonically)  
--
-- * @ot@: Face with ordered vertices                                                              
--
-- CONTRACT:
--
-- * for all @x :: ot@, @g .* vertices x == vertices (g .* x)@
--
-- * for all @x :: t@, @vertices x == vertices (packOrderedFace 'mempty' x)@
--
-- * 'packOrderedFace' and 'unpackOrderedFace' must be inverses of each other (in other words, @ot@ is isomorphic to @(VertexSymGroup t, t)@, but I leave open the possibility to use a more efficient representation in the future)
class ( Group (VertexSymGroup t)
      , LeftAction (VertexSymGroup t) (VertexTuple t)
      , LeftAction (VertexSymGroup t) ot
      , Vertices t  (VertexTuple t)
      , Vertices ot (VertexTuple t) 
      )
      
      => OrderableFace 
            t  
            ot 
            
            | t -> ot, ot -> t where

    type VertexSymGroup t
    type VertexTuple t
    unpackOrderedFace :: ot -> (VertexSymGroup t, t)
    packOrderedFace :: (VertexSymGroup t) -> t -> ot


defaultVerticesForOrderedFace :: OrderableFace t ot => ot -> VertexTuple t
defaultVerticesForOrderedFace (unpackOrderedFace -> (g,x)) = g .* vertices x

defaultLeftActionForOrderedFace :: OrderableFace t ot => VertexSymGroup t -> ot -> ot
defaultLeftActionForOrderedFace g2 (unpackOrderedFace -> (g1,x)) = packOrderedFace (g2 .*. g1) x

forgetVertexOrder ::  OrderableFace t ot => ot -> t
forgetVertexOrder = snd . unpackOrderedFace

-- | Equivalent to 'packOrderedFace mempty'
toOrderedFace :: OrderableFace t ot => t -> ot
toOrderedFace = packOrderedFace mempty

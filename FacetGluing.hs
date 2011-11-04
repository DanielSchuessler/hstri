{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleInstances, ViewPatterns #-} 
module FacetGluing where

import AbstractTetrahedron
import IndexedSimplices

type Gluing = (ITriangle,OITriangle)

class GluingSpec a where
    type TetIndex a
    toGluing :: (TetIndex a -> TIndex) -> a -> Gluing

instance GluingSpec (t,Triangle,t,S3,Triangle) where
    type TetIndex (t,Triangle,t,S3,Triangle) = t
    toGluing f (tt1,t1,tt2,g,t2) = (f tt1 ./ t1, f tt2 ./ packOrderedFace g t2)

instance GluingSpec (t,Triangle,t,OTriangle) where
    type TetIndex (t,Triangle,t,OTriangle) = t
    toGluing f (tt1,t1,tt2,t2) = (f tt1 ./ t1, f tt2 ./ t2)

-- | Creates a gluing equivalent to the input gluing but with the 'ITriangle's swapped and the permutation inverted
flipGluing :: Gluing -> Gluing
flipGluing (x1, unpackOrderedFace -> (g,x2)) = (x2, packOrderedFace (inv g) x1)


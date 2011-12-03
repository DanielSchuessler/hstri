{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleInstances, ViewPatterns #-} 
module FacetGluing where

import AbstractTetrahedron
import IndexedSimplices
import Control.Applicative
import HomogenousTuples
import Data.Maybe

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

gluingMapVertex :: Gluing -> IVertex -> Maybe IVertex
gluingMapVertex (t,ot) v = triangleGetVertexAt ot <$> triangleGetIndexOf t v 

gluingUnmapVertex :: Gluing -> IVertex -> Maybe IVertex
gluingUnmapVertex (t,ot) v = triangleGetVertexAt t <$> triangleGetIndexOf ot v 

prop_gluingMapVertex :: Gluing -> Bool
prop_gluingMapVertex gluing@(t,ot) =
    ot == oiTriangleByVertices us
  where
    us = map3 (fromJust . gluingMapVertex gluing) (vertices t)



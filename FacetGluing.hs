{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TupleSections, NoMonomorphismRestriction, TypeFamilies, FlexibleInstances, ViewPatterns #-} 
module FacetGluing where

import AbstractTetrahedron
import IndexedSimplices
import Control.Applicative
import HomogenousTuples
import Data.Maybe
import INormalDisc
import Control.Exception
import Control.Monad

type Gluing = (ITriangle,OITriangle)

class GluingSpec a where
    type TetIndex a
    toGluing :: (TetIndex a -> TIndex) -> a -> Gluing


oiTriangleGluing :: OITriangle -> OITriangle -> Gluing
oiTriangleGluing (unpackOrderedFace -> (g,tri)) otri = (tri, inv g .* otri)

instance GluingSpec (t,Triangle,t,S3,Triangle) where
    type TetIndex (t,Triangle,t,S3,Triangle) = t
    toGluing f (tt1,t1,tt2,g,t2) = (f tt1 ./ t1, f tt2 ./ packOrderedFace g t2)

instance GluingSpec (t,Triangle,t,OTriangle) where
    type TetIndex (t,Triangle,t,OTriangle) = t
    toGluing f (tt1,t1,tt2,t2) = (f tt1 ./ t1, f tt2 ./ t2)

-- | Creates a gluing equivalent to the input gluing but with the 'ITriangle's swapped and the permutation inverted
flipGluing :: Gluing -> Gluing
flipGluing (x1, unpackOrderedFace -> (g,x2)) = (x2, packOrderedFace (inv g) x1)

class GluingMappable a where
    gluingMap :: Gluing -> a -> a

gluingUnmap g a = gluingMap (flipGluing g) a

instance GluingMappable IVertex where

    gluingMap (t,ot) v = 
        maybe (error ("gluingMap: vertex not in left triangle")) 
            (triangleGetVertexAt ot)  
            (triangleGetIndexOf t v)

instance GluingMappable Vertex where

    gluingMap (t,ot) v = 
        maybe (error ("gluingMap: vertex not in left triangle")) 
            (triangleGetVertexAt (forgetTIndex ot))  
            (triangleGetIndexOf (forgetTIndex t) v)


instance GluingMappable OIEdge where
    gluingMap gl@(t,ot) (viewI -> I i (vertices -> (v0,v1))) = 

        assert (i==getTIndex t) $
        
        getTIndex ot ./ oedge (gluingMap gl v0, gluingMap gl v1) 

instance GluingMappable IEdge where
    gluingMap gl = forgetVertexOrder . gluingMap gl . packOrderedFace NoFlip

instance GluingMappable INormalCorner where
    gluingMap gl = iNormalCorner . gluingMap gl . iNormalCornerGetContainingEdge

instance GluingMappable INormalArc where
    gluingMap gl@(tri,otri) arc =

        assert (isSubface arc tri) $
            iNormalArc . (otri,) . gluingMap gl . iNormalArcGetVertex $ arc

prop_gluingMapVertex :: Gluing -> Bool
prop_gluingMapVertex gluing@(t,ot) =
    ot == oiTriangleByVertices us
  where
    us = map3 (gluingMap gluing) (vertices t)






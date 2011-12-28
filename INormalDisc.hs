{-# LANGUAGE ViewPatterns, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module INormalDisc where

import NormalDisc
import HomogenousTuples
import ONormal
import Control.Exception
import AbstractTetrahedron()

type INormalCorner = I NormalCorner
type INormalArc = I NormalArc
type OINormalArc = I ONormalArc
type INormalDisc = I NormalDisc
type INormalTri = I NormalTri
type INormalQuad = I NormalQuad

trivialHasTIndexInstance [t|NormalCorner|]
trivialHasTIndexInstance [t|NormalArc|]
trivialHasTIndexInstance [t|ONormalArc|]
trivialHasTIndexInstance [t|NormalDisc|]
trivialHasTIndexInstance [t|NormalTri|]
trivialHasTIndexInstance [t|NormalQuad|]

class MakeINormalDisc a where
    iNormalDisc :: a -> INormalDisc

class MakeINormalTri a where
    iNormalTri :: a -> INormalTri

class MakeINormalQuad a where
    iNormalQuad :: a -> INormalQuad

class MakeINormalArc a where
    iNormalArc :: a -> INormalArc

class MakeINormalCorner a where
    iNormalCorner :: a -> INormalCorner

instance MakeINormalArc (ITriangle,Vertex) where
    iNormalArc (viewI -> I i t,v) = (./) i (normalArc (t, v))

instance MakeINormalArc (OITriangle,Vertex) where
    iNormalArc (t,v) = iNormalArc (forgetVertexOrder t,v)

instance MakeINormalArc (Triangle,IVertex) where
    iNormalArc (t,viewI -> I i v) = (./) i (normalArc (t, v))

instance MakeINormalArc (ITriangle,IVertex) where
    iNormalArc (viewI -> I i0 t,viewI -> I i v) = 
        assert (i0==i) $
        (./) i (normalArc (t, v))

instance MakeINormalArc (OITriangle,IVertex) where
    iNormalArc (t,v) = iNormalArc (forgetVertexOrder t,v)

instance NormalCorners INormalArc (Pair INormalCorner) where
    normalCorners = traverseI map2 normalCorners

instance MakeINormalTri IVertex where
    iNormalTri = mapI normalTri

instance MakeINormalDisc INormalTri where
    iNormalDisc = mapI normalDisc

instance MakeINormalDisc INormalQuad where
    iNormalDisc = mapI normalDisc


instance NormalCorners TIndex (Sextuple INormalCorner) where
    normalCorners ti = map6 (ti ./) allNormalCorners'

instance NormalCorners ITriangle (Triple INormalCorner) where
    normalCorners = traverseI map3 normalCorners

instance NormalArcs TIndex [INormalArc] where
    normalArcs ti = map (ti ./) allNormalArcs

instance NormalTris TIndex (Quadruple INormalTri) where
    normalTris ti = map4 (ti ./) allNormalTris' 

instance NormalQuads TIndex (Triple INormalQuad) where
    normalQuads ti = map3 (ti ./) allNormalQuads' 

instance NormalDiscs TIndex [INormalDisc] where
    normalDiscs ti = map (ti ./) allNormalDiscs

-- | Normal triangles meeting a given edge
instance NormalTris IEdge (Pair INormalTri) where
    normalTris = map2 iNormalTri . vertices 

-- | Normal triangles meeting a given edge
instance NormalTris OIEdge (Pair INormalTri) where
    normalTris = map2 iNormalTri . vertices 

instance NormalArcs INormalDisc [INormalArc] where
    normalArcs = traverseI fmap normalArcs

instance NormalArcs INormalTri (Triple INormalArc) where
    normalArcs = traverseI map3 normalArcs

instance NormalArcs INormalQuad (Quadruple INormalArc) where
    normalArcs = traverseI map4 normalArcs

instance NormalArcs ITriangle (Triple INormalArc) where
    normalArcs = traverseI map3 normalArcs

-- | Constructs a normal quad specified by one of the two edges disjoint from it (in the same tetrahedron as the given edge)
iNormalQuadByDisjointEdge :: IEdge -> INormalQuad
iNormalQuadByDisjointEdge = mapI normalQuadByDisjointEdge 

instance MakeINormalCorner IEdge where
    iNormalCorner = mapI normalCorner

instance MakeINormalCorner OIEdge where
    iNormalCorner = mapI normalCorner

iNormalArcGetVertex :: INormalArc -> IVertex
iNormalArcGetVertex = mapI normalArcGetVertex 

iNormalArcGetVertexIndex
  :: I NormalArc -> Maybe VertexIndexInTriangle
iNormalArcGetVertexIndex = normalArcGetVertexIndex . unI

iNormalArcGetTriangle :: INormalArc -> ITriangle
iNormalArcGetTriangle = mapI normalArcGetTriangle 

iNormalArcByTriangleAndVertexIndex
  :: ITriangle -> VertexIndexInTriangle -> I NormalArc
iNormalArcByTriangleAndVertexIndex (viewI -> I ti t) vi = 
    ti ./ normalArcByTriangleAndVertexIndex t vi
    

eitherIND
  :: (INormalTri -> c) -> (INormalQuad -> c) -> INormalDisc -> c
eitherIND kt kq (viewI -> I i x) = eitherND (kt . (i ./)) (kq . (i ./)) x

iNormalTriByNormalArc :: INormalArc -> INormalTri
iNormalTriByNormalArc = mapI normalTriByNormalArc

iNormalQuadByNormalArc :: INormalArc -> INormalQuad
iNormalQuadByNormalArc = mapI normalQuadByNormalArc

iNormalCornerGetContainingEdge :: INormalCorner -> IEdge
iNormalCornerGetContainingEdge = mapI normalCornerGetContainingEdge


instance IsSubface INormalArc ITriangle where isSubface = liftIsSubface
instance IsSubface INormalCorner ITriangle where isSubface = liftIsSubface
instance IsSubface INormalCorner IEdge where isSubface = liftIsSubface

instance MakeINormalDisc INormalDisc where
    iNormalDisc = id


iNormalTriGetVertex :: INormalTri -> IVertex
iNormalTriGetVertex = mapI normalTriGetVertex

iNormalQuadGetDisjointEdges :: I NormalQuad -> Pair IEdge
iNormalQuadGetDisjointEdges = traverseI map2 normalQuadGetDisjointEdges

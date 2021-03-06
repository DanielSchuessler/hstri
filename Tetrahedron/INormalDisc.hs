{-# LANGUAGE FlexibleContexts, TypeFamilies, ScopedTypeVariables, ViewPatterns, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Tetrahedron.INormalDisc where

import Tetrahedron.NormalDisc
import HomogenousTuples
import Control.Exception
import Tetrahedron()
import Control.Applicative

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
    iNormalArc = uncurry liftMakeNormalArc

instance MakeINormalArc (OITriangle,IVertex) where
    iNormalArc (t,v) = iNormalArc (forgetVertexOrder t,v)


instance NormalCorners INormalArc (Pair INormalCorner) where
    normalCorners = traverseI map2 normalCorners

instance NormalCorners TIndex (Sextuple INormalCorner) where
    normalCorners ti = map6 (ti ./) allNormalCorners'

instance NormalCorners ITriangle (Triple INormalCorner) where
    normalCorners = traverseI map3 normalCorners

instance NormalCorners INormalTri (Triple INormalCorner) where
    normalCorners = traverseI map3 normalCorners
    
instance NormalCorners INormalQuad (Quadruple INormalCorner) where
    normalCorners = iNormalQuadGetNormalCornersInOrder

instance NormalCorners INormalDisc [INormalCorner] where
    normalCorners = traverseI map normalCorners

instance MakeINormalTri IVertex where
    iNormalTri = mapI normalTri

instance MakeINormalDisc INormalTri where
    iNormalDisc = iNormalTriToINormalDisc

instance MakeINormalDisc INormalQuad where
    iNormalDisc = iNormalQuadToINormalDisc



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

-- | Get the vertex enclosed (in the 'iNormalArcGetTriangle') by representatives of this normal arc type
iNormalArcGetVertex :: INormalArc -> IVertex
iNormalArcGetVertex = mapI normalArcGetVertex 

-- | Get the index of the 'iNormalArcGetVertex' in the 'iNormalArcGetTriangle'
iNormalArcGetVertexIndex
  :: I NormalArc -> Index3
iNormalArcGetVertexIndex = normalArcGetVertexIndex . unI

-- | Get the triangle containing representatives of this normal arc type
iNormalArcGetTriangle :: INormalArc -> ITriangle
iNormalArcGetTriangle = mapI normalArcGetTriangle 

-- | Get the triangle containing representatives of this normal arc type
oiNormalArcGetTriangle :: OINormalArc -> ITriangle
oiNormalArcGetTriangle = mapI oNormalArcGetTriangle 

-- | = 'iNormalArcGetTriangle'
instance MakeITriangle INormalArc where
    iTriangle = iNormalArcGetTriangle


liftMakeNormalArc
  :: (HasTIndex ia t, HasTIndex ia1 t1, MakeNormalArc (t, t1)) =>
     ia -> ia1 -> I NormalArc
liftMakeNormalArc (viewI -> I ti t) (viewI -> I ti' v) = 
    assert(ti==ti') $
    ti ./ normalArc (t, v) 

iNormalArcByTriangleAndVertex
  :: ITriangle -> IVertex -> INormalArc
iNormalArcByTriangleAndVertex = liftMakeNormalArc

iNormalArcByNormalCorners :: INormalCorner -> INormalCorner -> I NormalArc
iNormalArcByNormalCorners = liftMakeNormalArc

iNormalArcByOTriangleAndVertex
  :: OITriangle -> IVertex -> I NormalArc
iNormalArcByOTriangleAndVertex = iNormalArcByTriangleAndVertex . forgetVertexOrder

iNormalArcByTriangleAndVertexIndex
  :: ITriangle -> Index3 -> I NormalArc
iNormalArcByTriangleAndVertexIndex = traverseI (.) normalArcByTriangleAndVertexIndex

iNormalArcByOITriangleAndVertexIndex
  :: I OTriangle -> Index3 -> I NormalArc
iNormalArcByOITriangleAndVertexIndex = traverseI (.) normalArcByOTriangleAndVertexIndex
    
-- | Case analysis on whether a given 'INormalDisc' is a tri or a quad
eitherIND
  :: (INormalTri -> r) -> (INormalQuad -> r) -> INormalDisc -> r
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

iNormalQuadGetDisjointEdges :: INormalQuad -> Pair IEdge
iNormalQuadGetDisjointEdges = traverseI map2 normalQuadGetDisjointEdges

iNormalArcsAroundVertex :: IVertex -> Triple INormalArc
iNormalArcsAroundVertex = traverseI map3 normalArcsAroundVertex

iNormalDiscsContainingNormalArc
  :: INormalArc -> Pair (INormalDisc)
iNormalDiscsContainingNormalArc = traverseI map2 normalDiscsContainingNormalArc

isTri :: INormalDisc -> Bool
isTri = eitherIND (const True) (const False)

getShapeI :: (HasTIndex ib b, IsDiscShape b) => ib -> DiscShape b
getShapeI = getShape . forgetTIndex


iNormalDiscsContainingNormalCorner
  :: INormalCorner -> Quadruple INormalDisc
iNormalDiscsContainingNormalCorner = traverseI map4 normalDiscsContainingNormalCorner 

adjacentINormalCorners
  :: NormalCorner -> INormalDisc -> Pair INormalCorner
adjacentINormalCorners = traverseI map2 <$> adjacentNormalCorners


iNormalQuadToINormalDisc :: INormalQuad -> INormalDisc
iNormalQuadToINormalDisc (x :: INormalQuad) = mapI normalDisc x

iNormalTriToINormalDisc :: INormalTri -> INormalDisc
iNormalTriToINormalDisc (x :: INormalTri) = mapI normalDisc x 
 
iNormalQuadByVertexAndITriangle
  :: Vertex -> ITriangle -> INormalQuad
iNormalQuadByVertexAndITriangle = mapI . normalQuadByVertexAndTriangle

instance Vertices INormalArc where
    type Verts INormalArc = Pair INormalCorner
    vertices = traverseI map2 vertices

instance Vertices OINormalArc where
    type Verts OINormalArc = Pair INormalCorner
    vertices = traverseI map2 vertices

instance RightAction S2 OINormalArc where x *. g = mapI (*. g) x

instance OrderableFace INormalArc OINormalArc where
    type VertexSymGroup INormalArc = S2
    packOrderedFace = defaultPackOrderedFaceI 
    unpackOrderedFace = defaultUnpackOrderedFaceI

-- | Elements (circularly) adjacent in the returned tuple are adjacent corners
iNormalQuadGetNormalCornersInOrder
  :: INormalQuad -> Quadruple INormalCorner
iNormalQuadGetNormalCornersInOrder = traverseI map4 normalQuadGetNormalCornersInOrder

iNormalQuadGetNormalArcsInOrder
  :: INormalQuad -> Quadruple INormalArc
iNormalQuadGetNormalArcsInOrder = traverseI map4 normalQuadGetNormalArcsInOrder




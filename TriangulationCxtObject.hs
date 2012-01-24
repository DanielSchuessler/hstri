{-# LANGUAGE TupleSections, BangPatterns, NoMonomorphismRestriction, ImplicitParams, TypeFamilies, TypeOperators, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, MagicHash, Rank2Types, TypeSynonymInstances, ExistentialQuantification, NamedFieldPuns, RecordWildCards, ScopedTypeVariables, ViewPatterns, CPP, EmptyDataDecls, DeriveFunctor #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module TriangulationCxtObject(
    module Element,
    module Triangulation,
    module Tetrahedron.NormalDisc,
    module INormalDisc,



    -- * Generic
    T,getTriangulation,unT,
    pMap,
    preimageList,

    -- * Vertices
    TVertex,
    vertexEquivalence,
    -- ** Construction
    MakeTVertex(..),
    NormalArcPreimage(..),
    tVerticesOfIEdge,
    -- ** Properties
    equivalentIVertices,
    vertexLinkingSurfaceTris,
    dfsVertexLink,
    preimageListOfVertex,
    degreeOfVertex,

    -- * Edges
    TEdge,
    edgeEquivalence,
    -- ** Construction
    MakeTEdge(..),
    -- ** Properties
    equivalentIEdges,
    TEdgeIntersection(..),
    itrianglesContainingEdge,
    degreeOfEdge,
    preimageListOfEdge,
    -- ** Misc
    prettyEdgeEquivalence,

    -- * Triangles
    TTriangle,
    triangleEquivalence,
    iTriangleEqv,
    -- ** Construction
    MakeTTriangle(..),
    boundaryTriangles,
    innerTriangles,
    -- ** Properties
    isBoundaryTriangle,
    preimageListOfTriangle,
    eqvTriangles,


    -- * Normal corners
    TNormalCorner,
    -- ** Construction
    MakeTNormalCorner(..),
    -- ** Properties

    -- * Normal arcs
    TNormalArc,
    -- ** Construction
    MakeTNormalArc(..),
    boundaryNormalArcs,
    innerNormalArcs,
    tNormalArcsAroundVertex,
    -- ** Properties
    normalArcPreimage,
    isBoundaryNormalArc,

    -- * Normal dics
    TNormalTri,
    TNormalQuad,
    TNormalDisc,

    -- * For testing
    isSubface_ET,isSubface_VE,isSubface_TTet
    
    ) where


import AbstractTetrahedron
import Collections
import Control.Exception
import Control.Monad.Reader
import Data.EdgeLabelledTree
import Data.Function
import Data.Functor
import Data.Maybe
import Element
import Equivalence
import HomogenousTuples
import INormalDisc
import Tetrahedron.NormalDisc
import Prelude hiding(catch,lookup)
import PrettyUtil
import Quote
import ShortShow
import Triangulation
import Triangulation.CanonOrdered
import Util

-- | INVARIANT: the 'unT' is a canonical representative of its equivalence class (under the gluing)
data T a = 
    -- | \"Unsafe\" due to invariant
    UnsafeMakeT {
        getTriangulation :: Triangulation,
        unT :: a
    }

-- | Vertex in the quotient space of a triangulation
type TVertex = T IVertex

-- | Edge in the quotient space of a triangulation
type TEdge = T IEdge

-- | Triangle in the quotient space of a triangulation
type TTriangle = T ITriangle

-- | Normal corner in the quotient space of a triangulation
type TNormalCorner = T INormalCorner

-- | Normal arc in the quotient space of a triangulation
type TNormalArc = T INormalArc

-- | N.B. normal tris are never identified; this type just carries the triangulation along
type TNormalTri = T INormalTri

-- | N.B. normal discs are never identified; this type just carries the triangulation along
type TNormalQuad = T INormalQuad

-- | N.B. normal discs are never identified; this type just carries the triangulation along
type TNormalDisc = T INormalDisc

-- class TriangulationCxtObject a pre | a -> pre, pre -> a where
--     preimage :: T a -> pre



preimageList :: IsEquivalenceClass (T a) => T a -> [Element (T a)]
preimageList = asList

preimageListOfVertex :: TVertex -> [IVertex]
preimageListOfVertex y = eqvEquivalents (vertexEqv (getTriangulation y)) (unT y)
degreeOfVertex :: TVertex -> Int
degreeOfVertex y = ecSize (eqvClassOf (vertexEqv (getTriangulation y)) (unT y))

type instance Element TVertex = IVertex

vertexEquivalence :: Triangulation -> EnumEqvImpl TVertex
vertexEquivalence tr = enumEqvImpl (pMap tr) (vertices tr)


instance AsList TVertex where
    asList = preimageListOfVertex

instance IsEquivalenceClass TVertex where
    canonicalRep = unT
    ecSize = degreeOfVertex

type instance Element TEdge = COIEdge

tEdgeOIEdgeClass :: TEdge -> SetBasedEquivalenceClass OIEdge
tEdgeOIEdgeClass (y :: TEdge) = 
    eqvClassOf 
        (oEdgeEqv . getTriangulation $ y) 
        (toOrderedFace . unT $ y)


preimageListOfEdge :: TEdge -> [COIEdge]
preimageListOfEdge = fmap UnsafeCanonOrdered . asList . tEdgeOIEdgeClass 
    -- ok because @toOrderedFace (unT y)@ is the representative of its class

instance AsList TEdge where
    asList = preimageListOfEdge

degreeOfEdge :: TEdge -> Int
degreeOfEdge y = ecSize (tEdgeOIEdgeClass y)

instance IsEquivalenceClass TEdge where
    canonicalRep = UnsafeCanonOrdered . toOrderedFace . unT
--     ecMember x y = 
--         ecMember (unCanonOrdered x) (tEdgeOIEdgeClass y)

    ecSize = degreeOfEdge 
            

edgeEquivalence :: Triangulation -> EnumEqvImpl TEdge
edgeEquivalence tr = enumEqvImpl (pMap tr . forgetVertexOrder . unCanonOrdered) (edges tr)

    

type instance Element TTriangle = COITriangle


preimageListOfTriangle :: TTriangle -> [COITriangle]
preimageListOfTriangle y =
      let
        tri = unT y
      in
        fmap UnsafeCanonOrdered . 
            maybe [toOrderedFace tri] (\(otri) -> [toOrderedFace tri, otri])
                $
                    lookupGluingOfTTriangle y

instance AsList TTriangle where
    asList = preimageListOfTriangle

instance IsEquivalenceClass TTriangle where
    canonicalRep = UnsafeCanonOrdered . toOrderedFace . unT

--     ecMember x y = 
--         forgetVertexOrder (unCanonOrdered x) == unT y
--         ||
--         Just (unCanonOrdered x) == lookupGluingOfTTriangle y

    ecSize = maybe 1 (const 2) . lookupGluingOfTTriangle 


triangleEquivalence :: Triangulation -> EnumEqvImpl TTriangle
triangleEquivalence tr =
    enumEqvImpl 
        (pMap tr . forgetVertexOrder . unCanonOrdered)
        (triangles tr)

iTriangleEqv :: Triangulation -> EnumEqvImpl (EqvClassImpl ITriangle)
iTriangleEqv tr = 
    let
        f = forgetVertexOrder . unCanonOrdered
    in
        enumEqvImpl
            (\t -> ecMap f (pMap tr t))
            (fmap (ecMap f) (triangles tr)) 
        


class MakeTVertex a where tvertex :: a -> TVertex
class MakeTEdge a where tedge :: a -> TEdge
class MakeTTriangle a where ttriangle :: a -> TTriangle
class MakeTNormalCorner a where tnormalCorner :: a -> TNormalCorner
class MakeTNormalArc a where tnormalArc :: a -> TNormalArc

instance MakeTEdge TNormalCorner where
    tedge te = let
                t = getTriangulation te 
                I i (edge -> e) = viewI (unT te)
               in
                UnsafeMakeT t (i ./ e) 

normalCornerPreimage :: MakeTEdge a => a -> [INormalCorner]
normalCornerPreimage = fmap iNormalCorner . equivalentIEdges . tedge


instance MakeTNormalCorner TEdge where
    tnormalCorner (UnsafeMakeT t e)= UnsafeMakeT t (iNormalCorner e)



             
    

instance Vertices Triangulation where
    type Verts Triangulation = [TVertex]

    vertices t = fmap (UnsafeMakeT t . canonicalRep) (eqvClasses (vertexEqv t))

instance Edges Triangulation where
    type Eds Triangulation = [TEdge] 

    edges t = fmap (UnsafeMakeT t . canonicalRep) 
                (eqvClasses (iEdgeEqv t))

instance Triangles Triangulation where
    type Tris Triangulation = [TTriangle] 

    triangles t = nub' (pMap t <$> tITriangles t)

instance NormalCorners Triangulation [TNormalCorner] where
    normalCorners t = tnormalCorner <$> edges t

instance NormalArcs Triangulation [TNormalArc] where
    normalArcs t = concatMap normalArcList $ triangles t

instance NormalTris Triangulation [T INormalTri] where
    normalTris t = fmap (UnsafeMakeT t) . concatMap normalTriList . tTetrahedra_ $ t

instance NormalQuads Triangulation [T INormalQuad] where
    normalQuads t = fmap (UnsafeMakeT t) . concatMap normalQuadList . tTetrahedra_ $ t


equivalentIVertices :: TVertex -> [IVertex]
equivalentIVertices = preimageList

equivalentIEdges :: TEdge -> [OIEdge]
equivalentIEdges = fmap unCanonOrdered . preimageList








instance Show TVertex where showsPrec = prettyShowsPrec
instance Show TEdge where showsPrec = prettyShowsPrec
instance Show TTriangle where showsPrec = prettyShowsPrec
instance Show TNormalCorner where showsPrec = prettyShowsPrec
instance Show TNormalArc where showsPrec = prettyShowsPrec
instance Show TNormalTri where showsPrec = prettyShowsPrec
instance Show TNormalQuad where showsPrec = prettyShowsPrec
instance Show TNormalDisc where showsPrec = prettyShowsPrec

instance ShortShow TVertex where 
    shortShow = shortShowAsSet

instance  Pretty TVertex where
    pretty = prettyListAsSet . preimageList

instance  Pretty TEdge where
    pretty = prettyListAsSet . preimageList

instance  Pretty TTriangle where
    pretty = prettyListAsSet . preimageList

instance  Pretty TNormalCorner where
    pretty = prettyListAsSet . normalCornerPreimage

instance  Pretty TNormalArc where
    pretty = prettyListAsSet . asList . normalArcPreimage

instance  Pretty TNormalTri where
    pretty = pretty . unT

instance  Pretty TNormalQuad where
    pretty = pretty . unT

instance  Pretty TNormalDisc where
    pretty = pretty . unT

                
                


boundaryTriangles ::  Triangulation -> [TTriangle]
boundaryTriangles = filter isBoundaryTriangle . triangles

innerTriangles ::  Triangulation -> [TTriangle]
innerTriangles = filter (not . isBoundaryTriangle) . triangles


isBoundaryTriangle :: TTriangle -> Bool
isBoundaryTriangle = (==1) . ecSize

                








-- | This instance assumes the invariant that the @a@ in the @T a@ is always a canonical representative of its equivalence class!
instance (Eq a) => Eq (T a) where 
    (==) = (==) `on` unT

-- | This instance assumes the invariant that the @a@ in the @T a@ is always a canonical representative of its equivalence class!
instance (Ord a) => Ord (T a) where 
    compare = compare `on` unT





isSubface_VE :: TVertex -> IEdge -> Bool
isSubface_VE x y = any2 (\x' -> x == p x') (vertices y)
    where
        p = pMap (getTriangulation x)

isSubface_ET ::  TEdge -> ITriangle -> Bool
isSubface_ET x y = any3 (\x' -> x == p x') (edges y)
    where
        p = pMap (getTriangulation x)

isSubface_TTet :: Triangulation -> ITriangle -> TIndex -> Bool
isSubface_TTet t x i  = any (`isSubface` i) (eqvTriangles t x :: [ITriangle]) 

instance  IsSubface TVertex TEdge where
    isSubface x y = isSubface_VE x (unT y)

instance  IsSubface TEdge TTriangle where
    isSubface x y = isSubface_ET x (unT y)

instance IsSubface TTriangle TIndex where
    isSubface x i = isSubface_TTet (getTriangulation x) (unT x) i







eqvTriangles :: Triangulation -> ITriangle -> [ITriangle]
eqvTriangles t x = x : case lookup x (tGlueMap_ t) of
                          Just x' -> [forgetVertexOrder x']
                          Nothing -> []






-- -- | This instance is trivial since tetrahedra are never glued together
-- instance CanonicalRep TIndex TIndex where canonicalRep = getTIndex



tVerticesOfIEdge :: Triangulation -> IEdge -> Pair TVertex
tVerticesOfIEdge t rep = map2 (pMap t) (vertices rep)

instance Vertices TEdge where 
    type Verts TEdge = Pair TVertex
    vertices e = tVerticesOfIEdge (getTriangulation e) (unT e)

instance Edges TTriangle  where 
    type Eds TTriangle = Triple (TEdge)
    edges (UnsafeMakeT t rep) = map3 (pMap t) (edges rep)

instance Triangles (Triangulation, TIndex)  where
    type Tris (Triangulation,TIndex) = Quadruple (TTriangle)
    triangles (t,i) = map4 (pMap t) (triangles i)



lookupGluingOfTTriangle :: TTriangle -> Maybe OITriangle
lookupGluingOfTTriangle (UnsafeMakeT t rep) = lookup rep (tGlueMap_ t)

instance NormalArcs (T INormalTri) (Triple TNormalArc) where
    normalArcs (UnsafeMakeT t x) = map3 (pMap t) (normalArcs x) 

instance NormalArcs (T INormalQuad) (Quadruple TNormalArc) where
    normalArcs (UnsafeMakeT t x) = map4 (pMap t) (normalArcs x) 

eitherTND :: (T INormalTri -> c) -> (T INormalQuad -> c) -> T INormalDisc -> c
eitherTND kt kq (UnsafeMakeT t x) = eitherIND (kt . UnsafeMakeT t) (kq . UnsafeMakeT t) x

instance NormalArcs (T INormalDisc) [TNormalArc] where
    normalArcs = eitherTND normalArcList normalArcList

instance NormalArcs TTriangle (Triple TNormalArc) where
    normalArcs (UnsafeMakeT t x) = map3 (UnsafeMakeT t) (normalArcs x)




instance NormalCorners TNormalArc (Pair TNormalCorner) where
    normalCorners (UnsafeMakeT t x) = map2 (pMap t) (normalCorners x)


vertexLinkingSurfaceTris :: TVertex -> [INormalTri]
vertexLinkingSurfaceTris = fmap iNormalTri . preimageListOfVertex



-- | NB: Having the same endpoints does not imply equality for 'TEdge's (our triangulations needn't be simplicial complexes)
data TEdgeIntersection =
    TEI_Empty |
    TEI_Vertex TVertex |
    TEI_TwoVertices TVertex TVertex |
    TEI_Same

instance Intersection TEdge TEdge TEdgeIntersection where

    intersect e1 e2 | e1 == e2 = TEI_Same

    intersect e1 e2 = case (intersect2_2 `on` vertices) e1 e2 of
                            [] -> TEI_Empty
                            [v] -> TEI_Vertex v
                            [v0,v1] -> TEI_TwoVertices v0 v1
                            _ -> assert False undefined


-- | Maps a thing from the disjoint union of tetrahedra of a triangulation to its image in the quotient space
pMap
  :: TriangulationDSnakeItem a =>
     Triangulation -> a -> T a
pMap t x = UnsafeMakeT t (canonicalize t x)

-- instance MakeTVertex (Triangulation, IVertex) where
--     tvertex (t, x) = UnsafeMakeT t (canonicalizeIVertex t x) 
-- 
-- instance MakeTEdge (Triangulation, IEdge) where
--     tedge (t, x) = UnsafeMakeT t (canonicalizeIEdge t x) 
-- 
-- instance MakeTTriangle (Triangulation, ITriangle) where
--     ttriangle (t, rep) = UnsafeMakeT t (canonicalizeITriangle t rep)
--                         
-- instance MakeTNormalCorner (Triangulation, INormalCorner) where
--     tnormalCorner (t,x) = UnsafeMakeT t (canonicalizeINormalCorner t x) 
-- 
-- instance MakeTNormalArc (Triangulation, INormalArc) where
--     tnormalArc (t, ina) = UnsafeMakeT t (canonicalizeINormalArc t ina) 


data NormalArcPreimage =
        BoundaryNormalArc INormalArc
    |   InnerNormalArc INormalArc INormalArc
    deriving Show


normalArcPreimage :: TNormalArc -> NormalArcPreimage
normalArcPreimage (UnsafeMakeT tr x) = case gluedNormalArc tr x of
                                 Nothing -> BoundaryNormalArc x
                                 Just x' -> InnerNormalArc x x'

type instance Element NormalArcPreimage = INormalArc

instance AsList NormalArcPreimage where
    asList (BoundaryNormalArc x) = [x]
    asList (InnerNormalArc x y) = [x,y]

isBoundaryNormalArc :: TNormalArc -> Bool
isBoundaryNormalArc na = case normalArcPreimage na of 
                             BoundaryNormalArc {} -> True
                             _ -> False

innerNormalArcs :: Triangulation -> [TNormalArc]
innerNormalArcs = filter (not . isBoundaryNormalArc) . normalArcs

boundaryNormalArcs :: Triangulation -> [TNormalArc]
boundaryNormalArcs = filter isBoundaryNormalArc . normalArcs






        
dfsVertexLink :: TVertex -> EdgeLabelledTree INormalTri (Pair INormalArc)
dfsVertexLink v = dfs nt0 (adjacentNormalTris (getTriangulation v)) 
    where
        nt0 :: INormalTri
        nt0 = iNormalTri (unT v)
        


itrianglesContainingEdge :: TEdge -> [ITriangle]
itrianglesContainingEdge =
    concatMap (\e -> toList2 (star (forgetVertexOrder e) (TwoSkeleton AbsTet))) . 
        equivalentIEdges
    
-- | = 'vertexLinkingSurfaceTris'
instance NormalTris TVertex [INormalTri] where
    normalTris = vertexLinkingSurfaceTris

iNormalArcsAroundTVertex :: TVertex -> [INormalArc]
iNormalArcsAroundTVertex =
 concatMap (asList . iNormalArcsAroundVertex) . preimageList 

tNormalArcsAroundVertex :: TVertex -> [TNormalArc]
tNormalArcsAroundVertex v = 
    nub' . fmap p . iNormalArcsAroundTVertex $ v
  where
    p = pMap (getTriangulation v)



prettyEdgeEquivalence :: Triangulation -> Doc
prettyEdgeEquivalence = prettyEquivalence . edgeEquivalence

instance Pretty Triangulation where
    pretty tr =
        prettyRecord "Triangulation" fields

          where
            fields = [ ("Quote", text (quote tr))
                     , ("Number of tetrahedra", pretty (tNumberOfTetrahedra_ tr))
                     , ("Triangle gluings", pretty (tOriginalGluings tr))
                     , ("Canonically ordered edges", 
                            prettyEdgeEquivalence tr)
                     , ("Vertices", pretty (vertexEqv tr))
                     ]

instance Show Triangulation where
    showsPrec = prettyShowsPrec 


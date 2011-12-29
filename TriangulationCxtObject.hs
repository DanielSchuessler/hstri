{-# LANGUAGE TupleSections, BangPatterns, NoMonomorphismRestriction, ImplicitParams, TypeFamilies, TypeOperators, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, MagicHash, Rank2Types, TypeSynonymInstances, ExistentialQuantification, NamedFieldPuns, RecordWildCards, ScopedTypeVariables, ViewPatterns, CPP, EmptyDataDecls, DeriveFunctor #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
module TriangulationCxtObject(
    module Element,
    module Triangulation,
    module NormalDisc,
    module INormalDisc,

    T,getTriangulation,unT,
    TVertex,TEdge,TTriangle,TNormalCorner,TNormalArc,TNormalTri,TNormalQuad,TNormalDisc,
    pMap,

    -- * Preimages
    preimageList,
    NormalArcPreimage(..),
    normalArcPreimage,

    MakeTVertex(..),
    MakeTEdge(..),
    MakeTTriangle(..),
    MakeTNormalCorner(..),
    MakeTNormalArc(..),
    boundaryTriangles,
    innerTriangles,
    isBoundaryTriangle,
    equivalentIVertices,
    equivalentIEdges,
    vertexLinkingSurfaceTris,
    TEdgeIntersection(..),
    isBoundaryNormalArc,
    boundaryNormalArcs,
    innerNormalArcs,
    dfsVertexLink,
    itrianglesContainingEdge,
    
    -- * Testing
    qc_TriangulationCxtObject 
    ) where

import AbstractTetrahedron
import Collections
import Control.Exception
import Control.Monad.Reader
import Data.Function
import Data.Functor
import Data.Maybe
import Element
import Equivalence
import HomogenousTuples
import INormalDisc
import NormalDisc
import Prelude hiding(catch,lookup)
import PrettyUtil
import QuickCheckUtil
import Test.QuickCheck
import Test.QuickCheck.All
import Triangulation
import TupleTH
import UPair
import ShortShow
import Data.Graph.Inductive.Graph hiding(edges)
import Control.Arrow((&&&))

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

asList_T f y = eqvEquivalents (f (getTriangulation y)) (unT y)
canonicalRep_T = unT
ecMember_T f x y = eqvRep (f (getTriangulation y)) x == unT y
ecSize_T f y = ecSize (eqvClassOf (f (getTriangulation y)) (unT y))

type instance Element TVertex = IVertex

instance HasEquivalence Triangulation TVertex IVertex where
    eqvClassOf = pMap 
    eqvClasses = vertices

instance AsList TVertex where
    asList = asList_T vertexEqv 

instance IsEquivalenceClass TVertex where
    canonicalRep = canonicalRep_T
    ecMember = ecMember_T vertexEqv 
    ecSize = ecSize_T vertexEqv

type instance Element TEdge = COIEdge

tEdgeOIEdgeClass :: TEdge -> SetBasedEquivalenceClass OIEdge
tEdgeOIEdgeClass (y :: TEdge) = 
    eqvClassOf 
        (oEdgeEqv . getTriangulation $ y) 
        (toOrderedFace . unT $ y)

instance AsList TEdge where
    asList = 
        -- ok because @toOrderedFace (unT y)@ is the representative of its class
        fmap UnsafeCanonOrdered . asList . tEdgeOIEdgeClass 


instance IsEquivalenceClass TEdge where
    canonicalRep = UnsafeCanonOrdered . toOrderedFace . unT
    ecMember x y = 
        ecMember (unCanonOrdered x) (tEdgeOIEdgeClass y)

    ecSize y = ecSize (tEdgeOIEdgeClass y)
            
instance HasEquivalence Triangulation TEdge COIEdge where 
    eqvClassOf tr = pMap tr . forgetVertexOrder . unCanonOrdered 
    eqvClasses = edges

type instance Element TTriangle = COITriangle

instance AsList TTriangle where
    asList y =
      let
        tri = unT y
      in
        fmap UnsafeCanonOrdered . 
            maybe [toOrderedFace tri] (\(otri) -> [toOrderedFace tri, otri])
                $
                    lookupGluingOfTTriangle y

instance IsEquivalenceClass TTriangle where
    canonicalRep = UnsafeCanonOrdered . toOrderedFace . unT

    ecMember x y = 
        forgetVertexOrder (unCanonOrdered x) == unT y
        ||
        Just (unCanonOrdered x) == lookupGluingOfTTriangle y

    ecSize = maybe 1 (const 2) . lookupGluingOfTTriangle 

instance HasEquivalence Triangulation TTriangle COITriangle where
    eqvClassOf tr = pMap tr . forgetVertexOrder . unCanonOrdered

    eqvClasses = triangles
                                      



class MakeTVertex a where tvertex :: a -> TVertex
class MakeTEdge a where tedge :: a -> TEdge
class MakeTTriangle a where ttriangle :: a -> TTriangle
class MakeTNormalCorner a where tnormalCorner :: a -> TNormalCorner
class MakeTNormalArc a where tnormalArc :: a -> TNormalArc

instance MakeTEdge TNormalCorner where
    tedge (UnsafeMakeT t (viewI -> I i (edge -> e))) = UnsafeMakeT t (i ./ e) 

normalCornerPreimage = fmap iNormalCorner . equivalentIEdges . tedge

-- instance TriangulationCxtObject IVertex (SetBasedEquivalenceClass IVertex) where
--     preimage (UnsafeMakeT t x) = eqvClassOf (vertexEqv t) x
-- 
-- 
-- instance TriangulationCxtObject IEdge (SetBasedEquivalenceClass IEdge) where
--     preimage (UnsafeMakeT t x) = eqvClassOf (edgeEqv t) x
-- 
-- 
-- instance TriangulationCxtObject ITriangle (Maybe Gluing) where
--     preimage (UnsafeMakeT t x) =  







instance MakeTNormalCorner TEdge where
    tnormalCorner (UnsafeMakeT t e)= UnsafeMakeT t (iNormalCorner e)



             
    

instance Vertices Triangulation [TVertex] where
    vertices t = fmap (UnsafeMakeT t . canonicalRep) (eqvClasses (vertexEqv t))

instance Edges Triangulation [TEdge] where
    edges t = fmap (UnsafeMakeT t . canonicalRep) (eqvClasses (edgeEqv t))

instance Triangles Triangulation [TTriangle] where
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


prop_VerticesOfEdge_welldefined :: Triangulation -> Property
prop_VerticesOfEdge_welldefined t = forAllElements (unT <$> edges t)
            (mkWellDefinednessProp (eqvEquivalents (edgeEqv t))
               (uncurry uPair . vertices_E t)) 
                
                


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
isSubface_VE x y = any2 (\x' -> x == pMap (getTriangulation x) x') (vertices y)

isSubface_ET ::  TEdge -> ITriangle -> Bool
isSubface_ET x y = $(anyTuple 3) (\x' -> x == pMap (getTriangulation x) x') (edges y)

isSubface_TTet :: Triangulation -> ITriangle -> TIndex -> Bool
isSubface_TTet t x i  = any (`isSubface` i) (eqvTriangles t x :: [ITriangle]) 

instance  IsSubface TVertex TEdge where
    isSubface x y = isSubface_VE x (unT y)

instance  IsSubface TEdge TTriangle where
    isSubface x y = isSubface_ET x (unT y)

instance IsSubface TTriangle TIndex where
    isSubface x i = isSubface_TTet (getTriangulation x) (unT x) i




prop_IsSubface_TTet_welldefined :: Triangulation -> Property
prop_IsSubface_TTet_welldefined t = 
    forAllElements2 (unT <$> triangles t) (tTetrahedra_ t)
        (mkWellDefinednessProp2 (eqvTriangles t) ((:[]))
            (isSubface_TTet t))  

prop_IsSubface_VE_welldefined :: Triangulation -> Property
prop_IsSubface_VE_welldefined t = 
    forAllElements2 (unT <$> vertices t) (unT <$> edges t)
        (mkWellDefinednessProp2 (eqvEquivalents (vertexEqv t)) (eqvEquivalents (edgeEqv t))
            (\x y -> isSubface_VE (pMap t x) y))  



prop_IsSubface_ET_welldefined :: Triangulation -> Property
prop_IsSubface_ET_welldefined t = 
    forAllElements2 (unT <$> edges t) (unT <$> triangles t)
        (mkWellDefinednessProp2 (eqvEquivalents (edgeEqv t)) (eqvTriangles t)
            (\x y -> isSubface_ET (pMap t x) y))  




eqvTriangles :: Triangulation -> ITriangle -> [ITriangle]
eqvTriangles t x = x : case lookup x (tGlueMap_ t) of
                          Just x' -> [forgetVertexOrder x']
                          Nothing -> []






-- -- | This instance is trivial since tetrahedra are never glued together
-- instance CanonicalRep TIndex TIndex where canonicalRep = getTIndex



vertices_E :: Triangulation -> IEdge -> Pair TVertex
vertices_E t rep = map2 (pMap t) (vertices rep)

instance Vertices (TEdge) (Pair TVertex) where 
    vertices (UnsafeMakeT t x) = vertices_E t x

instance Edges (TTriangle) (Triple (TEdge)) where 
    edges (UnsafeMakeT t rep) = map3 (pMap t) (edges rep)

instance Triangles (Triangulation, TIndex) (Quadruple (TTriangle)) where
    triangles (t,i) = map4 (pMap t) (triangles i)


prop_TIndexToTTriangles_surjective ::  Triangulation -> Property
prop_TIndexToTTriangles_surjective t = setEq (triangles t) (concatMap (triangleList . (t,)) (tTetrahedra_ t)) 

prop_TTrianglesToTEdges_surjective ::  Triangulation -> Property
prop_TTrianglesToTEdges_surjective t = setEq (edges t) (concatMap edgeList (triangles t)) 


prop_TEdgesToTVertices_surjective ::  Triangulation -> Property
prop_TEdgesToTVertices_surjective t = setEq (vertices t) (concatMap vertexList (edges t)) 

lookupGluingOfTTriangle :: TTriangle -> Maybe OITriangle
lookupGluingOfTTriangle (UnsafeMakeT t rep) = lookup rep (tGlueMap_ t)

qc_TriangulationCxtObject ::  IO Bool
qc_TriangulationCxtObject = $quickCheckAll

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


prop_normalArcsOfTriangulationAreCanonical :: Triangulation -> Property
prop_normalArcsOfTriangulationAreCanonical tr =
    forAll (elements (normalArcs tr))
        (\(UnsafeMakeT _ arc) -> arc == canonicalize tr arc)

prop_normalArcsOfTriangulationAreDistinct :: Triangulation -> Bool
prop_normalArcsOfTriangulationAreDistinct tr =
    not (hasDuplicates (normalArcs tr))


instance NormalCorners TNormalArc (Pair TNormalCorner) where
    normalCorners (UnsafeMakeT t x) = map2 (pMap t) (normalCorners x)


vertexLinkingSurfaceTris :: TVertex -> [INormalTri]
vertexLinkingSurfaceTris = fmap iNormalTri . preimageList



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

prop_normalArcs_triangles :: Triangulation -> Property
prop_normalArcs_triangles tr =
    length (innerNormalArcs tr) .=. 3 * length (innerTriangles tr) 
    .&.
    length (boundaryNormalArcs tr) .=. 3 * length (boundaryTriangles tr) 



-- | Returns the normal triangles adjacent to the given one, and the arcs witnessing the adjacencies (the first arc of each pair is the arc of the input triangle) 
adjacentNormalTris
  :: 
     Triangulation -> INormalTri -> [(Pair INormalArc, INormalTri)]
adjacentNormalTris tr nt =
    let
        arcs = normalArcList nt
    in
        mapMaybe (\arc -> do
            arc' <- gluedNormalArc tr arc
            return ((arc, arc'), iNormalTriByNormalArc arc'))

            arcs


        
dfsVertexLink :: TVertex -> EdgeLabelledTree INormalTri (Pair INormalArc)
dfsVertexLink v = dfs nt0 (adjacentNormalTris (getTriangulation v)) 
    where
        nt0 :: INormalTri
        nt0 = iNormalTri (unT v)
        

itrianglesContainingEdge :: TEdge -> [ITriangle]
itrianglesContainingEdge =
    concatMap (\e -> toList2 (star (forgetVertexOrder e) (TwoSkeleton AbsTet))) . 
        equivalentIEdges
    
instance NormalTris TVertex [INormalTri] where
    normalTris = map iNormalTri . asList 

tNormalArcsAroundVertex v = nub' ( 

vertexLinkGraph (v :: TVertex) = 
    mkGraph (map (fromEnum &&& id) (normalTris v)) [] 



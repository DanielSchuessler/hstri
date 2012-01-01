{-# LANGUAGE TupleSections, BangPatterns, NoMonomorphismRestriction, ImplicitParams, TypeFamilies, TypeOperators, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, MagicHash, Rank2Types, TypeSynonymInstances, ExistentialQuantification, NamedFieldPuns, RecordWildCards, ScopedTypeVariables, ViewPatterns, CPP, EmptyDataDecls, DeriveFunctor #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module TriangulationCxtObject(
    module Element,
    module Triangulation,
    module NormalDisc,
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

    -- * Triangles
    TTriangle,
    triangleEquivalence,
    -- ** Construction
    MakeTTriangle(..),
    boundaryTriangles,
    innerTriangles,
    -- ** Properties
    isBoundaryTriangle,
    preimageListOfTriangle,


    -- * Normal corners
    TNormalCorner,
    -- ** Construction
    MakeTNormalCorner(..),
    -- ** Properties

    -- * Normal arcs
    TNormalArc,
    InnNA(..),
    -- ** Construction
    MakeTNormalArc(..),
    boundaryNormalArcs,
    innerNormalArcs,
    tNormalArcsAroundVertex,
    toInnNA,
    innNAs,
    -- ** Properties
    normalArcPreimage,
    isBoundaryNormalArc,
    normalTrisContainingInnNA,

    -- * Normal dics
    TNormalTri,
    TNormalQuad,
    TNormalDisc,

    -- * Misc
    triArcGraph,
    
    -- * Testing
    qc_TriangulationCxtObject 
    ) where

import AbstractTetrahedron
import Collections
import Control.Arrow((&&&))
import Control.Exception
import Control.Monad.Reader
import Data.Function
import Data.Functor
import Data.Graph.Inductive.Graph hiding(edges)
import Data.Graph.Inductive.Tree
import Data.Maybe
import Element
import Equivalence
import HomogenousTuples
import INormalDisc
import NormalDisc
import Prelude hiding(catch,lookup)
import PrettyUtil
import QuickCheckUtil
import ShortShow
import Test.QuickCheck
import Test.QuickCheck.All
import Triangulation
import Triangulation.CanonOrdered
import UPair

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
ecMember_T :: IVertex -> TVertex -> Bool
ecMember_T x y = eqvRep (vertexEqv (getTriangulation y)) x == unT y
degreeOfVertex :: TVertex -> Int
degreeOfVertex y = ecSize (eqvClassOf (vertexEqv (getTriangulation y)) (unT y))

type instance Element TVertex = IVertex

vertexEquivalence :: Triangulation -> EnumEqvImpl TVertex
vertexEquivalence tr = enumEqvImpl (pMap tr) (vertices tr)

prop_vertexEquivalence :: Triangulation -> Property
prop_vertexEquivalence tr = polyprop_Equivalence' (vertexEquivalence tr) (tIVertices tr)

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

prop_edgeEquivalence :: Triangulation -> Property
prop_edgeEquivalence tr = polyprop_Equivalence' (edgeEquivalence tr) (tCOIEdges tr)
    

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
                                      
prop_triangleEquivalence :: Triangulation -> Property
prop_triangleEquivalence tr = polyprop_Equivalence' (triangleEquivalence tr) (tCOITriangles tr)

iTriangleEqv :: Triangulation -> EnumEqvImpl (EqvClassImpl ITriangle)
iTriangleEqv tr = 
    let
        f = forgetVertexOrder . unCanonOrdered
    in
        enumEqvImpl
            (\t -> ecMap f (pMap tr t))
            (fmap (ecMap f) (triangles tr)) 
        

prop_iTriangleEqv :: Triangulation -> Property
prop_iTriangleEqv tr = polyprop_Equivalence' (iTriangleEqv tr) (tITriangles tr)


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



             
    

instance Vertices Triangulation [TVertex] where
    vertices t = fmap (UnsafeMakeT t . canonicalRep) (eqvClasses (vertexEqv t))

instance Edges Triangulation [TEdge] where
    edges t = fmap (UnsafeMakeT t . canonicalRep) 
                (eqvClasses (iEdgeEqv t))

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
prop_VerticesOfEdge_welldefined tr = 
    polyprop_respects (iEdgeEqv tr) trivialEquivalence 
       (elements (tIEdges tr))
       (uncurry uPair . tVerticesOfIEdge tr) 
                
                


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




prop_IsSubface_TTet_welldefined :: Triangulation -> Property
prop_IsSubface_TTet_welldefined t = 
    forAllElements2 (unT <$> triangles t) (tTetrahedra_ t)
        (mkWellDefinednessProp2 (eqvTriangles t) ((:[]))
            (isSubface_TTet t))  

prop_IsSubface_VE_welldefined :: Triangulation -> Property
prop_IsSubface_VE_welldefined t = 
    forAllElements (vertices t) $ \v ->
    polyprop_respects (iEdgeEqv t) trivialEquivalence (elements $ tIEdges t)
        (isSubface_VE v)



prop_IsSubface_ET_welldefined :: Triangulation -> Property
prop_IsSubface_ET_welldefined t = 
    forAllElements (edges t) $ \e ->
        polyprop_respects 
            (iTriangleEqv t) 
            trivialEquivalence 
            (elements $ tITriangles t)
            (isSubface_ET e)





eqvTriangles :: Triangulation -> ITriangle -> [ITriangle]
eqvTriangles t x = x : case lookup x (tGlueMap_ t) of
                          Just x' -> [forgetVertexOrder x']
                          Nothing -> []






-- -- | This instance is trivial since tetrahedra are never glued together
-- instance CanonicalRep TIndex TIndex where canonicalRep = getTIndex



tVerticesOfIEdge :: Triangulation -> IEdge -> Pair TVertex
tVerticesOfIEdge t rep = map2 (pMap t) (vertices rep)

instance Vertices (TEdge) (Pair TVertex) where 
    vertices e = tVerticesOfIEdge (getTriangulation e) (unT e)

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

prop_normalArcs_triangles :: Triangulation -> Property
prop_normalArcs_triangles tr =
    length (innerNormalArcs tr) .=. 3 * length (innerTriangles tr) 
    .&.
    length (boundaryNormalArcs tr) .=. 3 * length (boundaryTriangles tr) 





        
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

tNormalArcsAroundVertex :: TVertex -> [TNormalArc]
tNormalArcsAroundVertex v = 
    nub' . fmap p . concatMap (asList . iNormalArcsAroundVertex) . asList $ v
  where
    p = pMap (getTriangulation v)

-- prop_tNormalArcsAroundVertex tr =
--     forAllElements (vertices tr) $ \v ->
--         setEq
--             (tNormalArcsAroundVertex v)
--             (filter 

normalTrisContainingInnNA :: InnNA -> Pair INormalTri
normalTrisContainingInnNA (InnNA ina1 ina2) = 
                        map2 iNormalTriByNormalArc (ina1, ina2)

data InnNA = InnNA { innNA_fst, innNA_snd :: INormalArc }
    deriving Show

toInnNA :: TNormalArc -> Maybe InnNA
toInnNA tna = 
    case normalArcPreimage tna of
                BoundaryNormalArc _ -> Nothing
                InnerNormalArc ina1 ina2 ->
                        Just (InnNA ina1 ina2)
                                    

innNAs :: Triangulation -> [InnNA]
innNAs = mapMaybe toInnNA . normalArcs

triArcGraph :: Triangulation -> Gr INormalTri InnNA
triArcGraph tr = 
--    undir $
    mkGraph 
        (map (fromEnum &&& id) (tINormalTris tr)) 
        (map f (innNAs tr)) 

  where
    f tna = 
        let
            (t1,t2) = normalTrisContainingInnNA tna
        in (fromEnum t1,fromEnum t2,tna)

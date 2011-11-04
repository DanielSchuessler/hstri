{-# LANGUAGE NoMonomorphismRestriction, ImplicitParams, TypeFamilies, TypeOperators, StandaloneDeriving, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances, GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances, ViewPatterns, BangPatterns #-}
-- {-# OPTIONS -ddump-splices #-}
{-# OPTIONS -Wall #-}
module AbstractTetrahedron(
    module S2,
    module S3,
    module Util,

    -- * Vertices
    Vertex,
    vA,vB,vC,vD,
    allVertices,allVertices',
    MakeVertex(..),
    Vertices(..),
    vertexList,
    -- * Edges
    Edge, eAB,eAC,eAD,eBC,eBD,eCD,
    allEdges,allEdges', edgeByOppositeVertexAndTriangle,
    MakeEdge(..),
    Edges(..),
    edgeList,
    oppositeEdge,
    -- * Triangles
    Triangle,
    tABC,tABD,tACD,tBCD,
    allTriangles,allTriangles',
    MakeTriangle(..),
    Triangles(..),
    triangleList,
    -- * Ordered faces
    FaceType(..), defaultLeftActionForOrderedFace,
    forgetVertexOrder,
    IsSubface(..),
    -- ** Ordered edges
    OEdge,
    verticesToOEdge,
    HasOEdges(..),
    MakeOEdge(..),
    -- ** Ordered triangles
    OTriangle,
    verticesToOTriangle,
    MakeOTriangle(..),


    -- * Testing
    qc_AbstractTetrahedron)
    
    where

import Collections
import Control.Exception
import Control.Monad.RWS
import Element
import HomogenousTuples
import Prelude hiding(catch,lookup)
import S2
import S3
import Test.QuickCheck
import Test.QuickCheck.All
import Text.PrettyPrint.ANSI.Leijen hiding((<$>))
import Util
import Data.Word
import Data.BitSet.Word8 as BitSet
import Language.Haskell.TH.Syntax as Syntax
import Vertex
import Edge
import Triangle

abstractTetrahedronColor ::  Doc -> Doc
abstractTetrahedronColor = cyan




-- | Things that have a collection of vertices
class AsList vertexTuple => Vertices a vertexTuple | a -> vertexTuple where
    vertices :: a -> vertexTuple

vertexList :: Vertices a vertexTuple => a -> [Element vertexTuple]
vertexList = asList . vertices

-- | Things that have a collection of edges
class AsList edgeTuple => Edges a edgeTuple | a -> edgeTuple where
    edges :: a -> edgeTuple

edgeList ::  Edges a b => a -> [Element b]
edgeList = asList . edges

class AsList triangleTuple => Triangles a triangleTuple | a -> triangleTuple where
    triangles :: a -> triangleTuple

triangleList ::  (Triangles a b) => a -> [Element b]
triangleList = asList . triangles















-- | Triangles containing a given edge
instance Triangles Edge (Pair Triangle) where
    triangles e = fromList2 ( filter4 (e `isSubface`) allTriangles' ) 


-- | Vertices contained in a given edge
instance Vertices Edge (Pair Vertex) where
    vertices = edgeVertices


            

-- | Edges contained in a given triangle
instance Edges Triangle (Triple Edge) where
    edges t@(Triangle x) = map3 (\v -> Edge (BitSet.delete v x)) (v2,v0,v1)
        where
            (v0,v1,v2) = vertices t


-- | Vertices contained in a given triangle
instance Vertices Triangle (Triple Vertex) where
    vertices = triangleVertices







class IsSubface x y where
    isSubface :: x -> y -> Bool

instance IsSubface Vertex Edge where
    isSubface = isVertexOfEdge

instance IsSubface Vertex Triangle where
    isSubface = isVertexOfTriangle

instance IsSubface Edge Triangle where
    isSubface = isEdgeOfTriangle

prop_IsSubface_transitive :: Vertex -> Edge -> Triangle -> Property
prop_IsSubface_transitive v e f = (isSubface v e && isSubface e f) ==> isSubface v e  

prop_IsSubface_count_VE :: Vertex -> Bool
prop_IsSubface_count_VE v = length ( filter6 (v `isSubface`) allEdges' ) == 3

prop_IsSubface_count_VF :: Vertex -> Bool
prop_IsSubface_count_VF v = length ( filter4 (v `isSubface`) allTriangles' ) == 3

prop_IsSubface_count_EF :: Edge -> Bool
prop_IsSubface_count_EF e = length ( filter4 (e `isSubface`) allTriangles' ) == 2


-- | Gets the edge which is contained in the given triangle and does /not/ contain the given vertex. 
--
-- The vertex must be contained in the triangle.
--
-- Example: @edgeByOppositeVertexAndTriangle 'B' 'ABD' = 'AD'@
edgeByOppositeVertexAndTriangle :: Vertex -> Triangle -> Edge
edgeByOppositeVertexAndTriangle v f =
    case filter3 (\e -> not (isSubface v e)) (edges f) of

         [e0] -> e0
         _ -> error ("edgeByOppositeVertexAndTriangle is not defined for args "++show v++", "++show f)


prop_edgeByOppositeVertexAndTriangle :: Vertex -> Triangle -> Property
prop_edgeByOppositeVertexAndTriangle v t | isSubface v t = (isSubface e t .&. not (isSubface v e)) 
                                         | otherwise = expectFailure (seq e True)
    where
        e = edgeByOppositeVertexAndTriangle v t

    




-- | Edges containing a given vertex
instance Edges Vertex (Triple Edge) where
    edges !v = fromList3 (filter6 (isSubface v) allEdges')

-- | Triangles containing a given vertex
instance Triangles Vertex (Triple Triangle) where
    triangles !v = fromList3 (filter4 (isSubface v) allTriangles')


prop_edgesContainingVertex :: Vertex -> Bool
prop_edgesContainingVertex v = all3 (isSubface v) (edges v)

prop_trianglesContainingVertex :: Vertex -> Bool
prop_trianglesContainingVertex v = all3 (isSubface v) (triangles v)

qc_AbstractTetrahedron ::  IO Bool
qc_AbstractTetrahedron = $(quickCheckAll)



class Intersection a b c | a b -> c where
    intersect :: a -> b -> c

instance Intersection Edge Edge (Maybe (Either Edge Vertex)) where
    intersect = intersectEdges 

instance Intersection Triangle Triangle (Either Triangle Edge) where
    intersect t1 t2 = case filter3 (\v -> elem3 v (edges t2)) (edges t1) of
                           [e] -> assert (t1/=t2) (Right e)
                           [_,_,_] -> assert (t1==t2) (Left t1)
                           _ -> assert False (error "impossible")







---- Ordered faces ----

-- | Oriented edge of an abstract tetrahedron
newtype OEdge = OEdge Word8 {- The lower nibble encodes the first vertex, the upper nibble encodes the second vertex. Invariant: The vertices are distinct. -}
    deriving(Eq,Ord)

instance Enum OEdge where 
    fromEnum (unpackOrderedFace -> (g,e)) = fromEnum (EnumPair g e)
    toEnum n = packOrderedFace g (e::Edge) where EnumPair g e = toEnum n


prop_EnumOEdge :: Property
prop_EnumOEdge = forAll (elements [0..11]) (\n -> fromEnum (toEnum n :: OEdge) .=. n)

instance Bounded OEdge where
    minBound = verticesToOEdge (vA,vB)
    maxBound = verticesToOEdge (vD,vC)

-- | Triangle of an abstract tetrahedron, with ordered vertices
data OTriangle = OTriangle !S3 !Triangle deriving(Eq,Ord)


class MakeOTriangle a where
    otriangle :: a -> OTriangle

instance MakeOTriangle Triangle where
    otriangle = curry otriangle S3abc

instance MakeOTriangle (S3,Triangle) where
    otriangle = uncurry packOrderedFace 



instance Lift OTriangle where
    lift (OTriangle g x) = [| OTriangle $(Syntax.lift g) $(Syntax.lift x) |]



class AsList oedgeTuple => HasOEdges a oedgeTuple | a -> oedgeTuple where
    oedges :: a -> oedgeTuple

instance HasOEdges OTriangle (Triple OEdge) where
    oedges (vertices -> (v0,v1,v2)) = (oedge (v0,v1), oedge (v1,v2), oedge(v2,v0)) 


class MakeOEdge a where
    oedge :: a -> OEdge


instance MakeOEdge (Pair Vertex) where
    oedge = verticesToOEdge

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
      
      => FaceType 
            t  
            ot 
            
            | t -> ot, ot -> t where

    type VertexSymGroup t
    type VertexTuple t
    unpackOrderedFace :: ot -> (VertexSymGroup t, t)
    packOrderedFace :: (VertexSymGroup t) -> t -> ot


defaultVerticesForOrderedFace :: FaceType t ot => ot -> VertexTuple t
defaultVerticesForOrderedFace (unpackOrderedFace -> (g,x)) = g .* vertices x

defaultLeftActionForOrderedFace :: FaceType t ot => VertexSymGroup t -> ot -> ot
defaultLeftActionForOrderedFace g2 (unpackOrderedFace -> (g1,x)) = packOrderedFace (g2 .*. g1) x

forgetVertexOrder ::  FaceType t ot => ot -> t
forgetVertexOrder = snd . unpackOrderedFace




instance FaceType Edge OEdge where 
    type VertexSymGroup Edge = S2
    type VertexTuple Edge = Pair Vertex

    unpackOrderedFace (vertices -> (v0,v1)) =
      let
        e = edge (v0,v1)
      in
        case compare v0 v1 of
             LT -> (NoFlip, e)
             GT -> (Flip  , e)
             EQ -> error ("Invalid OEdge with vertices "++show (v0,v1))

    packOrderedFace g e = verticesToOEdge (g .* vertices e) 

instance LeftAction S2 OEdge where
    (.*) = defaultLeftActionForOrderedFace

instance Show OEdge where
    show (vertices -> (v0,v1)) = show v0 ++ show v1


instance FaceType Triangle OTriangle where
    type VertexSymGroup Triangle = S3
    type VertexTuple Triangle = Triple Vertex
    unpackOrderedFace (OTriangle g x) = (g,x)
    packOrderedFace = OTriangle


instance LeftAction S3 OTriangle where
    (.*) = defaultLeftActionForOrderedFace



-- | Ordered edges contained in a given facet, in order
instance Edges OTriangle (OEdge,OEdge,OEdge) where
    edges x = case vertices x of
                   (v0,v1,v2) -> ( verticesToOEdge (v0,v1)
                                 , verticesToOEdge (v1,v2)
                                 , verticesToOEdge (v2,v0)
                                 )
    


instance MakeOTriangle (Triple Vertex) where
    otriangle = verticesToOTriangle

-- | Inverse function to @vertices :: O (Triangle) -> Triple Vertex@
verticesToOTriangle ::  (Vertex, Vertex, Vertex) -> OTriangle
verticesToOTriangle (v0, v1, v2)
        = case compare v0 v1 of
               LT -> case0LT1 v0 v1 v2
               EQ -> err
               _ -> 
                     -- let y = case0LT1 v1 v0 v2 
                     -- By correctness of case0LT1, we have: 
                     -- > vertices y = (v1,v0,v2) 
                     -- so,
                     -- > vertices (S3bac .* y) = S3bac .* (v1,v0,v2) = (v0,v1,v2)
                     -- as desired
                    S3bac .* case0LT1 v1 v0 v2

    where
        err = error (unwords ["verticesToOTriangle",show v0, show v1, show v2])

        case0LT1 w0 w1 w2 =
            assert (w0 < w1) $
                case compare w1 w2 of
                     LT -> caseOrdered w0 w1 w2
                     EQ -> err
                     _ -> S3acb .* case0LT2_1LT2 w0 w2 w1

        case0LT2_1LT2 w0 w1 w2 =
            assert (w0 < w2 && w1 < w2) $
                case compare w0 w1 of
                     LT -> caseOrdered w0 w1 w2
                     EQ -> err
                     _ -> S3bac .* caseOrdered w1 w0 w2


        caseOrdered (Vertex w0) (Vertex w1) (Vertex w2) = packOrderedFace S3abc $
                case (w0,w1,w2) of
                     (0,1,2) -> tABC
                     (0,1,3) -> tABD
                     (0,2,3) -> tACD
                     (1,2,3) -> tBCD
                     _ -> error (unwords ["impossible:","caseOrdered",show w0, show w1, show w2])

prop_VerticesToOTriangle ::  Vertex -> Property
prop_VerticesToOTriangle v0 =
    forAll (arbitrary `suchThat` (/= v0)) $ \v1 ->
    forAll (arbitrary `suchThat` (liftM2 (&&) (/= v0) (/= v1))) $ \v2 ->
        
        let vs = (v0,v1,v2) in vs .=. vertices (verticesToOTriangle vs) 

-- | Make an oriented edge of an abstract tetrahedron from two of its vertices (which must be distinct)
verticesToOEdge ::  (Vertex, Vertex) -> OEdge
verticesToOEdge (Vertex v0, Vertex v1) = OEdge (nibblesToWord8 (v0,v1))

prop_VerticesToOEdge ::  Vertex -> Property
prop_VerticesToOEdge v0 =
    forAll (arbitrary `suchThat` (/= v0)) $ \v1 ->
        let vs = (v0,v1) in vs .=. vertices (verticesToOEdge vs) 



prop_MakeEdge :: (Vertex,Vertex) -> Property
prop_MakeEdge vs@(v0,v1) = v0 < v1 ==> (vertices (edge vs) == vs)
                            

-- | Vertices contained in a given ordered facet (in order) 
--
-- > vertices (OTriangle g x) = g .* vertices x 
instance Vertices OTriangle (Triple Vertex) where
    vertices = defaultVerticesForOrderedFace

instance Vertices OEdge (Pair Vertex) where
    vertices (OEdge (word8ToNibbles -> (x1,x0))) = (Vertex x1, Vertex x0)

instance Show OTriangle where
    show = vertexTripleToString . vertices
    

instance Pretty OEdge where pretty = abstractTetrahedronColor . text . show

instance Pretty OTriangle where pretty = abstractTetrahedronColor . text . show


instance Arbitrary OEdge where arbitrary = liftM2 packOrderedFace arbitrary arbitrary 
instance Arbitrary OTriangle where arbitrary = liftM2 packOrderedFace arbitrary arbitrary 




-- instance Enum OEdge where
--     toEnum n = case toEnum n of EnumPair a b -> OEdge a b
--     fromEnum (OEdge a b) = fromEnum (EnumPair a b)

instance Enum OTriangle where
    toEnum n = case toEnum n of EnumPair a b -> OTriangle a b
    fromEnum (OTriangle a b) = fromEnum (EnumPair a b)


instance Bounded OTriangle where 
    minBound = OTriangle minBound minBound
    maxBound = OTriangle maxBound maxBound

instance Finite OEdge
instance Finite OTriangle

deriveCollectionKeyClass ''Edge
deriveCollectionKeyClass ''Triangle
deriveCollectionKeyClass ''OEdge
deriveCollectionKeyClass ''OTriangle



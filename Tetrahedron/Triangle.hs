{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies,BangPatterns,MultiParamTypeClasses, StandaloneDeriving, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Tetrahedron.Triangle(
    module Tetrahedron.Edge,

    -- * Plain
    Triangle,
    tBCD,tACD,tABD,tABC,
    allTriangles,allTriangles',
    -- ** Construction
    MakeTriangle(..),
    triangleByDualVertex,
    ascVerticesToTriangle,
    trianglesContainingVertex,
    trianglesContainingEdge,
    verticesToTriangle,
    triangleByVertices,
    joinVertexAndEdge,

    -- ** Properties
    verticesOfTriangle,
    edgesOfTriangle,
    oEdgesOfTriangle,
    edgeByOppositeVertexAndTriangle,
    vertexByOppositeEdge,
    triangleDualVertex,
    isVertexOfTriangle,
    isEdgeOfTriangle,
    triangleMemo,

    -- ** Vertex indices
    VertexIndexInTriangle(..),
    triangleGetVertexAt,
    triangleGetEdgeAt,
    triangleGetIndexOf,
    allTriangleVertexIndices,
    allTriangleVertexIndices',

    -- * Ordered
    OTriangle,
    -- ** Properties
    oTriangleDualVertex,
    -- ** Construction
    MakeOTriangle(..),
    oTriangleByVertices,
    verticesToOTriangle,
    allOTriangles,

    -- * Indexed
    ITriangle,
    -- ** Properties
    iTriangleDualVertex,
    iVerticesOfTriangle,
    -- ** Construction
    MakeITriangle(..),
    iTriangleByVertices,
    iVerticesToITriangle,
    iVerticesToTriangle,
    verticesToITriangle,
    iTriangleByDualVertex,
    joinIVertexAndEdge,

    -- * Ordered and indexed
    OITriangle,
    -- ** Properties
    oiTriangleDualVertex,
    -- ** Construction
    MakeOITriangle(..),
    oiTriangleByVertices,
    verticesToOITriangle,
    iVerticesToOITriangle,

    -- * Testing
    qc_Triangle,


    ) where


import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Binary
import Data.Binary.Derive
import Data.List as List
import Data.Maybe
import Data.Proxy
import Element
import GHC.Generics(Generic)
import HomogenousTuples
import Language.Haskell.TH.Syntax as Syntax
import Math.Groups.S3
import OrderableFace
import PrettyUtil
import QuickCheckUtil
import Quote
import ShortShow
import Test.QuickCheck
import Test.QuickCheck.All
import Tetrahedron.Edge
import Util


-- | Triangle of an abstract tetrahedron (vertices unordered) 
newtype Triangle = Triangle { triangleDualVertex :: Vertex }
    deriving (Eq,Binary)

triangleByDualVertex :: Vertex -> Triangle
triangleByDualVertex = Triangle

instance Show Triangle where
    show (verticesOfTriangle -> (v0,v1,v2)) = show v0 ++ show v1 ++ show v2

-- | By 'triangleDualVertex' (= descending lexicographic by 'verticesOfTriangle')
deriving instance Ord Triangle 

instance Enum Triangle where
    toEnum n = triangleByDualVertex (toEnum (3-n) :: Vertex)
    fromEnum t = 3 - fromEnum (triangleDualVertex t) 

instance Bounded Triangle where
    minBound = tABC
    maxBound = tBCD


-- | Memoize a triangle-consuming function
triangleMemo :: (Triangle -> c) -> Triangle -> c
triangleMemo f = vertexMemo (f . Triangle) . triangleDualVertex

-- | Vertices contained in a given triangle, ascending
verticesOfTriangle :: Triangle -> (Triple Vertex)
verticesOfTriangle = otherVertices . triangleDualVertex


prop_verticesOfTriangle :: Triangle -> Property
prop_verticesOfTriangle t =
    asList (verticesOfTriangle t)  
    .=.
    sort (filter (`isVertexOfTriangle` t) allVertices)
              

tABC, tABD, tACD, tBCD :: Triangle
tABC = triangleByDualVertex vD
tABD = triangleByDualVertex vC
tACD = triangleByDualVertex vB
tBCD = triangleByDualVertex vA

allTriangles' ::  (Triangle, Triangle, Triangle, Triangle)
allTriangles' =  ( tABC , tABD , tACD , tBCD )

allTriangles ::  [Triangle]
allTriangles =  asList allTriangles'

trianglePrettyColor :: Doc -> Doc
trianglePrettyColor = cyan

instance Pretty Triangle where pretty = trianglePrettyColor . text . show

class MakeTriangle a where
    triangle :: a -> Triangle

-- | The vertices must be strictly ascending
ascVerticesToTriangle :: Triple Vertex -> Triangle
ascVerticesToTriangle vs = triangleByDualVertex . unviewVertex $
        case map3 viewVertex vs of
             (B,C,D) -> A
             (A,C,D) -> B
             (A,B,D) -> C
             (A,B,C) -> D
             _ -> error ("ascVerticesToTriangle "++show vs)

-- | The vertices must be distinct (but needn't be ordered)
verticesToTriangle :: (Triple Vertex) -> Triangle
verticesToTriangle = ascVerticesToTriangle . sort3

-- | = 'verticesToTriangle'
triangleByVertices :: (Triple Vertex) -> Triangle
triangleByVertices = verticesToTriangle

-- | = 'verticesToTriangle'
instance MakeTriangle (Triple Vertex) where
    triangle = verticesToTriangle

prop_MakeTriangle_VVV ::  Vertex -> Property
prop_MakeTriangle_VVV v0 = 
    forAll (elements vs') $ 
        \v1 -> forAll (elements (vs' \\ [v1])) $ 
            \v2 -> 
                let v012 = (v0,v1,v2) in asList v012 `setEq` (asList . verticesOfTriangle) (triangle v012)
    where
        vs' = allVertices \\ [v0]







-- | Construct a triangle containing the two edges
instance MakeTriangle (Pair Edge) where
    triangle (e1,e2) =
        case intersectEdges (oppositeEdge e1) (oppositeEdge e2) of
            Just (Right v) -> triangleByDualVertex v
            _ -> error ("triangle "++show (e1,e2))


prop_MakeTriangle_EE ::  Edge -> Edge -> Property
prop_MakeTriangle_EE e1 e2 = (e1 /= e2 && e1 /= oppositeEdge e2) ==> 
                                let
                                   t = triangle (e1,e2)
                                in
                                    (e1 `isEdgeOfTriangle` t) .&. (e2 `isEdgeOfTriangle` t)
                                    


isVertexOfTriangle :: Vertex -> Triangle -> Bool
isVertexOfTriangle v t = v /= triangleDualVertex t


isEdgeOfTriangle :: Edge -> Triangle -> Bool
isEdgeOfTriangle e t = isVertexOfEdge (triangleDualVertex t) (oppositeEdge e)

instance Arbitrary Triangle where arbitrary = elements allTriangles

instance Lift Triangle where
    lift (Triangle t) = [| Triangle $(Syntax.lift t) |]

instance Finite Triangle

instance Quote Triangle where
    quotePrec _ t = "t" ++ show t

-- | A 'Triangle' with a tetrahedron index attached to it
data ITriangle = ITriangle {-# UNPACK #-} !TIndex !Triangle
    deriving (Eq,Ord,Generic)

instance Binary ITriangle where
    put = derivePut
    get = deriveGet

instance HasTIndex ITriangle Triangle where
    viewI (ITriangle i x) = I i x
    (./) = ITriangle

instance Enum ITriangle where
    toEnum (toEnum -> I i x) = (./) i x
    fromEnum = fromEnum . viewI

instance Show ITriangle where show =  show . viewI
instance Quote ITriangle where quotePrec prec =  quotePrec prec . viewI
instance Pretty ITriangle where pretty = pretty . viewI
instance ShortShow ITriangle where shortShow = shortShow . viewI

-- | Triangle of an abstract tetrahedron, with ordered vertices
data OTriangle = OTriangle !Triangle !S3 
    deriving(Eq,Ord,Generic)
    

instance Binary OTriangle where
    put = derivePut
    get = deriveGet

class MakeOTriangle a where
    otriangle :: a -> OTriangle

instance MakeOTriangle Triangle where
    otriangle = toOrderedFace

instance MakeOTriangle (Triangle,S3) where
    otriangle = uncurry packOrderedFace 



instance Lift OTriangle where
    lift (OTriangle g x) = [| OTriangle $(Syntax.lift g) $(Syntax.lift x) |]

-- | An 'OTriangle' with a tetrahedron index attached to it
type OITriangle = I OTriangle

trivialHasTIndexInstance [t|OTriangle|]
trivialHasTIndexInstance [t|Pair OTriangle|]


instance OrderableFace ITriangle OITriangle where
    type VertexSymGroup ITriangle = S3
    unpackOrderedFace = defaultUnpackOrderedFaceI
    packOrderedFace = defaultPackOrderedFaceI

prop_OrderableFace_ITriangle :: Property
prop_OrderableFace_ITriangle = polyprop_OrderableFace (undefined :: Proxy ITriangle)



instance RightAction S3 OITriangle where (*.) = defaultRightActionForOrderedFace

instance OrderableFace Triangle OTriangle where
    type VertexSymGroup Triangle = S3
    unpackOrderedFace (OTriangle x g) = (x,g)
    packOrderedFace = OTriangle

prop_OrderableFace_Triangle :: Property
prop_OrderableFace_Triangle = polyprop_OrderableFace (undefined :: Proxy Triangle)


instance RightAction S3 OTriangle where
    (*.) = defaultRightActionForOrderedFace



-- | Ordered edges contained in a given triangle, in order
instance Edges OTriangle where
    type Eds OTriangle = Triple OEdge
    edges = oEdgesOfTriangle
    


instance MakeOTriangle (Triple Vertex) where
    otriangle = oTriangleByVertices

-- | Inverse function to @vertices :: O (Triangle) -> Triple Vertex@
oTriangleByVertices ::  (Triple Vertex) -> OTriangle
oTriangleByVertices vs =
    case sort3WithPermutation vs of
         (vs',g) -> packOrderedFace (ascVerticesToTriangle vs') g


trianglesContainingEdge :: Edge -> (Pair Triangle)
trianglesContainingEdge e = fromList2 ( filter4 (e `isEdgeOfTriangle`) allTriangles' ) 


gedgesOfTriangle
  :: (Vertices t, Verts t ~ Triple v) => ((v, v) -> e) -> t -> Triple e
gedgesOfTriangle f t = map3 f ((v0,v1),(v1,v2),(v2,v0)) 
        where
            (v0,v1,v2) = vertices t 


-- | Edges contained in a given triangle
edgesOfTriangle
  :: (Vertices t, Verts t ~ Triple v, MakeEdge (v, v)) => t -> Triple Edge
edgesOfTriangle = gedgesOfTriangle edge

-- | Ordered edges contained in a given triangle (result is a cycle)
oEdgesOfTriangle :: (Vertices t, Verts t ~ Triple Vertex) => t -> Triple OEdge
oEdgesOfTriangle = gedgesOfTriangle verticesToOEdge 


-- | = 'edgesOfTriangle'
instance Edges Triangle where
    type Eds Triangle = Triple Edge
    edges = edgesOfTriangle 

-- | = 'verticesOfTriangle'
instance Vertices Triangle where
    type Verts Triangle = Triple Vertex
    vertices = verticesOfTriangle

-- | Vertices contained in a given ordered facet (in order) 
--
-- > vertices (OTriangle g x) = g .* vertices x 
instance Vertices OTriangle where
    type Verts OTriangle = Triple Vertex
    vertices = defaultVerticesForOrderedFace

instance Show OTriangle where
    show = vertexTripleToString . vertices
instance Arbitrary OTriangle where arbitrary = liftM2 packOrderedFace arbitrary arbitrary 
instance Pretty OTriangle where pretty = abstractTetrahedronColor . text . show


instance Enum OTriangle where
    toEnum n = case toEnum n of EnumPair a b -> OTriangle a b
    fromEnum (OTriangle a b) = fromEnum (EnumPair a b)


instance Bounded OTriangle where 
    minBound = OTriangle minBound minBound
    maxBound = OTriangle maxBound maxBound


iVerticesOfTriangle
  :: (Vertices a, Verts a ~ Triple b, HasTIndex ia a, HasTIndex ib b) =>
     ia -> Triple ib
iVerticesOfTriangle = traverseI map3 vertices 

instance Finite OTriangle

-- | = 'iVerticesOfTriangle'
instance Vertices ITriangle where 
    type Verts ITriangle = Triple IVertex

    vertices = iVerticesOfTriangle

-- | = 'iVerticesOfTriangle'
instance Vertices OITriangle where 
    type Verts OITriangle = Triple IVertex
  
    vertices = iVerticesOfTriangle

instance Edges OITriangle where
    type Eds OITriangle = Triple OIEdge

    edges (viewI -> I i t) = map3 (i ./) (edges t)

instance Edges ITriangle where
    type Eds ITriangle = Triple IEdge
    edges (viewI -> I i t) = map3 (i ./) (edges t)


-- | Triangles containing a given vertex
trianglesContainingVertex
  :: Vertex -> Triple Triangle
trianglesContainingVertex = map3 triangleByDualVertex . otherVertices

prop_trianglesContainingVertex :: Vertex -> Property
prop_trianglesContainingVertex v =
   setEq 
    (asList (trianglesContainingVertex v))  
    (filter (isVertexOfTriangle v) allTriangles)


-- | Gets the edge which is contained in the given triangle and does /not/ contain the given vertex. 
--
-- The vertex must be contained in the triangle.
--
-- Example: @edgeByOppositeVertexAndTriangle 'B' 'ABD' = 'AD'@
edgeByOppositeVertexAndTriangle :: Vertex -> Triangle -> Edge
edgeByOppositeVertexAndTriangle v t =
    case filter3 (\e -> not (isVertexOfEdge v e)) (edges t) of

         [e0] -> e0
         _ -> error ("edgeByOppositeVertexAndTriangle is not defined for args "++show v++", "++show t)

-- | = 'edgeByOppositeVertexAndTriangle'
instance Link Vertex Triangle Edge where
    link = edgeByOppositeVertexAndTriangle

instance Link IVertex ITriangle IEdge where
    link (viewI -> I i v) (viewI -> I i' t) = 
        assert (i==i') (i ./ link v t)

vertexByOppositeEdge
  :: (Show a, Vertices a, Verts a ~ Triple Vertex) =>
     Edge -> a -> Vertex
vertexByOppositeEdge e t =
    case filter3 (\v -> not (isVertexOfEdge v e)) (vertices t) of

         [v0] -> v0
         _ -> error ("vertexByOppositeEdge is not defined for args "++show e++", "++show t)

-- | = 'vertexByOppositeEdge'
instance Link Edge Triangle Vertex where
    link = vertexByOppositeEdge

instance Link IEdge ITriangle IVertex where
    link (viewI -> I i e) (viewI -> I i' t) = 
        assert (i==i') (i ./ link e t)

instance Triangles TIndex where
    type Tris TIndex = Quadruple ITriangle
    triangles z = map4 (z ./) allTriangles'


instance OEdges OTriangle where
    type OEds OTriangle = Triple OEdge
    oedges (vertices -> (v0,v1,v2)) = (oedge (v0,v1), oedge (v1,v2), oedge(v2,v0)) 

data VertexIndexInTriangle = VT0 | VT1 | VT2
    deriving(Eq,Show,Enum)

allTriangleVertexIndices'
  :: (VertexIndexInTriangle,
      VertexIndexInTriangle,
      VertexIndexInTriangle)
allTriangleVertexIndices' = (VT0,VT1,VT2)
allTriangleVertexIndices :: [VertexIndexInTriangle]
allTriangleVertexIndices = asList allTriangleVertexIndices'

triangleGetEdgeAt :: Triangle -> VertexIndexInTriangle -> Edge
triangleGetEdgeAt t vi = edgeByOppositeVertexAndTriangle (triangleGetVertexAt t vi) t

triangleGetVertexAt
  :: (Vertices a, Verts a ~ Triple t) => a -> VertexIndexInTriangle -> t
triangleGetVertexAt (vertices -> (v0,v1,v2)) i = 
    case i of
         VT0 -> v0
         VT1 -> v1
         VT2 -> v2

triangleGetIndexOf
  :: (Eq a1, Vertices a, Verts a ~ Triple a1) =>
     a -> a1 -> Maybe VertexIndexInTriangle
triangleGetIndexOf (vertices -> (v0,v1,v2)) v 
    | v == v0 = Just VT0
    | v == v1 = Just VT1
    | v == v2 = Just VT2
    | otherwise = Nothing


oiTriangleByVertices
  :: Triple IVertex -> OITriangle
oiTriangleByVertices (map3 viewI -> (I i0 v0, I i1 v1, I i2 v2)) =
    assert (i1 == i0) $
    assert (i2 == i0) $
    i0 ./ otriangle (v0,v1,v2)


instance Arbitrary ITriangle where
    arbitrary = (./) <$> arbitrary <*> arbitrary

instance Quote OTriangle where
    quotePrec _ ot = "o"++show ot



qc_Triangle :: IO Bool
qc_Triangle = $quickCheckAll


oTriangleDualVertex :: OTriangle -> Vertex
oTriangleDualVertex = triangleDualVertex . forgetVertexOrder
iTriangleDualVertex :: ITriangle -> IVertex
iTriangleDualVertex = mapI triangleDualVertex
oiTriangleDualVertex :: I OTriangle -> IVertex
oiTriangleDualVertex = mapI oTriangleDualVertex

iTriangleByDualVertex :: IVertex -> ITriangle
iTriangleByDualVertex = mapI triangleByDualVertex


instance Star Vertex (OneSkeleton Triangle) (Pair Edge) where
    star v (OneSkeleton t) = case link v t of
                                  (vertices -> (v0,v1)) -> (edge (v,v0), edge (v,v1))

instance Star IVertex (OneSkeleton ITriangle) (Pair IEdge) where
    star (viewI -> I i v) (OneSkeleton (viewI -> I i' t)) = 
        assert (i==i')
         (map2 (i ./) (star v (OneSkeleton t)))

instance ShortShow Triangle where shortShow = show 


iTriangleByVertices :: Triple IVertex -> ITriangle
iTriangleByVertices = forgetVertexOrder . oiTriangleByVertices


-- | = 'triangleDualVertex'
instance Dual Triangle Vertex where
    dual = triangleDualVertex
    
-- | = 'triangleDualVertex'
instance Dual Vertex Triangle where
    dual = triangleByDualVertex

allOTriangles :: [OTriangle]
allOTriangles = [minBound..maxBound]

joinVertexAndEdge
  :: (Vertices edge, Verts edge ~ (Pair Vertex)) =>
     Vertex -> edge -> Triangle
joinVertexAndEdge v0 (vertices -> (v1,v2)) = verticesToTriangle (v0,v1,v2) 

joinIVertexAndEdge
  :: (Vertices edge, Verts edge ~ (Pair IVertex)) =>
     IVertex -> edge -> ITriangle
joinIVertexAndEdge v0 (vertices -> (v1,v2)) = iVerticesToITriangle (v0,v1,v2) 

-- | = 'iTriangleByVertices'
iVerticesToITriangle :: Triple IVertex -> ITriangle
iVerticesToITriangle = iTriangleByVertices

-- | = 'iTriangleByVertices'
iVerticesToTriangle :: Triple IVertex -> ITriangle
iVerticesToTriangle = iTriangleByVertices

-- | = 'iTriangleByVertices'
verticesToITriangle :: Triple IVertex -> ITriangle
verticesToITriangle = iTriangleByVertices

-- | = 'oTriangleByVertices'
verticesToOTriangle :: Triple Vertex -> OTriangle
verticesToOTriangle = oTriangleByVertices

-- | = 'oiTriangleByVertices'
verticesToOITriangle :: Triple IVertex -> OITriangle
verticesToOITriangle = oiTriangleByVertices

-- | = 'oiTriangleByVertices'
iVerticesToOITriangle :: Triple IVertex -> OITriangle
iVerticesToOITriangle = oiTriangleByVertices

class MakeITriangle a where
    iTriangle :: a -> ITriangle

-- | = 'iTriangleByVertices'
instance MakeITriangle (Triple IVertex) where
    iTriangle = iTriangleByVertices

-- | = 'forgetVertexOrder'
instance MakeITriangle (OITriangle) where
    iTriangle = forgetVertexOrder

class MakeOITriangle a where
    oiTriangle :: a -> OITriangle

-- | = 'oiTriangleByVertices'
instance MakeOITriangle (Triple IVertex) where
    oiTriangle = oiTriangleByVertices

-- | @uncurry 'packOrderedFace'@
instance MakeOITriangle (ITriangle,S3) where
    oiTriangle = uncurry packOrderedFace

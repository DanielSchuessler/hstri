{-# LANGUAGE FlexibleContexts, TypeFamilies,BangPatterns,MultiParamTypeClasses, StandaloneDeriving, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Triangle(
    module Edge,

    -- * Plain
    Triangle,
    tBCD,tACD,tABD,tABC,
    allTriangles,allTriangles',
    -- ** Construction
    MakeTriangle(..),
    triangleByDualVertex,
    ascVerticesToTriangle,

    -- ** Properties
    edgeByOppositeVertexAndTriangle,
    vertexByOppositeEdge,
    triangleDualVertex,
    isVertexOfTriangle,
    isEdgeOfTriangle,

    -- ** Vertex indices
    VertexIndexInTriangle(..),
    triangleGetVertexAt,
    triangleGetEdgeAt,
    triangleGetIndexOf,
    allTriangleVertexIndices,
    allTriangleVertexIndices',

    -- * Ordered
    OTriangle,
    MakeOTriangle(..),
    verticesToOTriangle,
    otriangleDualVertex,

    -- * Indexed
    ITriangle,
    itriangleDualVertex,

    -- * Ordered and indexed
    OITriangle,
    oiTriangleByVertices,
    oitriangleDualVertex,

    -- * Testing
    qc_Triangle,


    ) where


import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List as List
import Edge
import Element
import HomogenousTuples
import Language.Haskell.TH.Syntax as Syntax
import OrderableFace
import QuickCheckUtil
import PrettyUtil
import Quote
import S3
import Test.QuickCheck
import Util
import Test.QuickCheck.All
import ShortShow
import Data.Proxy


-- | Triangle of an abstract tetrahedron (vertices unordered) 
newtype Triangle = Triangle { triangleDualVertex :: Vertex }

triangleByDualVertex :: Vertex -> Triangle
triangleByDualVertex = Triangle

instance Show Triangle where
    show (triangleVertices -> (v0,v1,v2)) = show v0 ++ show v1 ++ show v2

deriving instance Eq Triangle 
deriving instance Ord Triangle 

instance Enum Triangle where
    toEnum n = triangleByDualVertex (toEnum (3-n) :: Vertex)
    fromEnum t = 3 - fromEnum (triangleDualVertex t) 

instance Bounded Triangle where
    minBound = tABC
    maxBound = tBCD

triangleVertices :: Triangle -> (Vertex, Vertex, Vertex)
triangleVertices t = fromList3 ( filter4 (`isVertexOfTriangle` t) allVertices' )
              

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

-- | The Vertices must be distinct (but needn't be ordered)
instance MakeTriangle (Vertex,Vertex,Vertex) where
    triangle = ascVerticesToTriangle . sort3

prop_MakeTriangle_VVV ::  Vertex -> Property
prop_MakeTriangle_VVV v0 = 
    forAll (elements vs') $ 
        \v1 -> forAll (elements (vs' \\ [v1])) $ 
            \v2 -> 
                let v012 = (v0,v1,v2) in asList v012 `setEq` (asList . triangleVertices) (triangle v012)
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
    deriving (Eq,Ord)

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
data OTriangle = OTriangle !Triangle !S3 deriving(Eq,Ord)


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
    type VertexTuple ITriangle = Triple IVertex
    unpackOrderedFace = defaultUnpackOrderedFaceI
    packOrderedFace = defaultPackOrderedFaceI

prop_OrderableFace_ITriangle :: Property
prop_OrderableFace_ITriangle = polyprop_OrderableFace (undefined :: Proxy ITriangle)



instance RightAction S3 OITriangle where (*.) = defaultRightActionForOrderedFace

instance OrderableFace Triangle OTriangle where
    type VertexSymGroup Triangle = S3
    type VertexTuple Triangle = Triple Vertex
    unpackOrderedFace (OTriangle x g) = (x,g)
    packOrderedFace = OTriangle

prop_OrderableFace_Triangle :: Property
prop_OrderableFace_Triangle = polyprop_OrderableFace (undefined :: Proxy Triangle)


instance RightAction S3 OTriangle where
    (*.) = defaultRightActionForOrderedFace



-- | Ordered edges contained in a given triangle, in order
instance Edges OTriangle (Triple OEdge) where
    edges x = case vertices x of
                   (v0,v1,v2) -> ( verticesToOEdge (v0,v1)
                                 , verticesToOEdge (v1,v2)
                                 , verticesToOEdge (v2,v0)
                                 )
    


instance MakeOTriangle (Triple Vertex) where
    otriangle = verticesToOTriangle

-- | Inverse function to @vertices :: O (Triangle) -> Triple Vertex@
verticesToOTriangle ::  (Vertex, Vertex, Vertex) -> OTriangle
verticesToOTriangle vs =
    case sort3WithPermutation vs of
         (vs',g) -> packOrderedFace (ascVerticesToTriangle vs') g

-- | Triangles containing a given edge
instance Star Edge (TwoSkeleton AbsTet) (Pair Triangle) where
    star e _ = fromList2 ( filter4 (e `isEdgeOfTriangle`) allTriangles' ) 

-- | Edges contained in a given triangle
instance Edges Triangle (Triple Edge) where
    edges t = map3 edge ((v0,v1),(v1,v2),(v2,v0)) 
        where
            (v0,v1,v2) = vertices t

-- | Vertices contained in a given triangle
instance Vertices Triangle (Triple Vertex) where
    vertices = triangleVertices

-- | Vertices contained in a given ordered facet (in order) 
--
-- > vertices (OTriangle g x) = g .* vertices x 
instance Vertices OTriangle (Triple Vertex) where
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

instance Finite OTriangle
instance Vertices ITriangle (Triple IVertex) where 
    vertices z = map3 ((./) (getTIndex z)) (vertices (forgetTIndex z)) 

instance Vertices OITriangle (Triple IVertex) where 
    vertices z = map3 ((./) (getTIndex z)) (vertices (forgetTIndex z)) 

instance Edges OITriangle (Triple OIEdge) where
    edges (viewI -> I i t) = map3 (i ./) (edges t)

instance Edges ITriangle (Triple IEdge) where
    edges (viewI -> I i t) = map3 (i ./) (edges t)


-- | Triangles containing a given vertex
instance Star Vertex (TwoSkeleton AbsTet) (Triple Triangle) where
    star v _ = fromList3 (filter4 (isVertexOfTriangle v) allTriangles')

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

-- | Equivalent to 'edgeByOppositeVertexAndTriangle'
instance Link Vertex Triangle Edge where
    link = edgeByOppositeVertexAndTriangle

instance Link IVertex ITriangle IEdge where
    link (viewI -> I i v) (viewI -> I i' t) = 
        assert (i==i') (i ./ link v t)

vertexByOppositeEdge
  :: (Show a, Vertices a (Triple Vertex)) =>
     Edge -> a -> Vertex
vertexByOppositeEdge e t =
    case filter3 (\v -> not (isVertexOfEdge v e)) (vertices t) of

         [v0] -> v0
         _ -> error ("vertexByOppositeEdge is not defined for args "++show e++", "++show t)

-- | Equivalent to 'vertexByOppositeEdge'
instance Link Edge Triangle Vertex where
    link = vertexByOppositeEdge

instance Link IEdge ITriangle IVertex where
    link (viewI -> I i e) (viewI -> I i' t) = 
        assert (i==i') (i ./ link e t)

instance Triangles TIndex (Quadruple ITriangle) where
    triangles z = map4 (z ./) allTriangles'

-- | Triangles containing a given vertex
instance Star IVertex (TwoSkeleton AbsTet) (Triple ITriangle) where
    star v p = traverseI map3 (flip star p) v

-- | Triangles containing a given edge
instance Star IEdge (TwoSkeleton AbsTet) (Pair ITriangle) where
    star e p = traverseI map2 (flip star p) e

instance OEdges OTriangle (Triple OEdge) where
    oedges (vertices -> (v0,v1,v2)) = (oedge (v0,v1), oedge (v1,v2), oedge(v2,v0)) 

data VertexIndexInTriangle = VT0 | VT1 | VT2
    deriving(Eq,Show)

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
  :: Vertices a (t, t, t) => a -> VertexIndexInTriangle -> t
triangleGetVertexAt (vertices -> (v0,v1,v2)) i = 
    case i of
         VT0 -> v0
         VT1 -> v1
         VT2 -> v2

triangleGetIndexOf
  :: (Eq a1, Vertices a (a1, a1, a1)) =>
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


otriangleDualVertex :: OTriangle -> Vertex
otriangleDualVertex = triangleDualVertex . forgetVertexOrder
itriangleDualVertex :: ITriangle -> IVertex
itriangleDualVertex = mapI triangleDualVertex
oitriangleDualVertex :: I OTriangle -> IVertex
oitriangleDualVertex = mapI otriangleDualVertex

itriangleByDualVertex :: IVertex -> ITriangle
itriangleByDualVertex = mapI triangleByDualVertex

-- Equivalent to 'triangleDualVertex'
instance Link Triangle AbsTet Vertex where 
    link t _ = triangleDualVertex t

-- Equivalent to 'itriangleDualVertex'
instance Link ITriangle AbsTet IVertex where 
    link t _ = itriangleDualVertex t

-- Equivalent to 'triangleByDualVertex'
instance Link Vertex AbsTet Triangle where 
    link v _ = triangleByDualVertex v

-- Equivalent to 'itriangleByDualVertex'
instance Link IVertex AbsTet ITriangle where 
    link v _ = itriangleByDualVertex v

instance Star Vertex (OneSkeleton Triangle) (Pair Edge) where
    star v (OneSkeleton t) = case link v t of
                                  (vertices -> (v0,v1)) -> (edge (v,v0), edge (v,v1))

instance Star IVertex (OneSkeleton ITriangle) (Pair IEdge) where
    star (viewI -> I i v) (OneSkeleton (viewI -> I i' t)) = 
        assert (i==i')
         (map2 (i ./) (star v (OneSkeleton t)))

instance ShortShow Triangle where shortShow = show 

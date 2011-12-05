{-# LANGUAGE FlexibleContexts, TypeFamilies,BangPatterns,MultiParamTypeClasses, StandaloneDeriving, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Triangle(
    module Edge,

    -- * Plain
    Triangle,
    allTriangles,allTriangles',
    MakeTriangle(..),
    edgeByOppositeVertexAndTriangle,
    vertexByOppositeEdge,
    isVertexOfTriangle,
    isEdgeOfTriangle,

    -- ** Vertex indices
    VertexIndexInTriangle(..),
    triangleGetVertexAt,
    triangleGetIndexOf,

    -- * Ordered
    OTriangle,
    MakeOTriangle(..),
    verticesToOTriangle,

    -- * Indexed
    ITriangle,

    -- * Ordered and indexed
    OITriangle,
    oiTriangleByVertices



    ) where

import Collections
import Control.Exception
import Data.BitSet.Word8 as BitSet
import Element
import Language.Haskell.TH.Syntax as Syntax
import Test.QuickCheck
import Text.PrettyPrint.ANSI.Leijen hiding((<$>))
import Util
import Vertex
import Data.List as List
import HomogenousTuples
import Edge
import Data.Monoid
import OrderableFace
import S3
import Control.Monad
import Control.Applicative



-- | Triangle of an abstract tetrahedron (vertices unordered) 
newtype Triangle = Triangle (BitSet Vertex)

instance Show Triangle where
    show (triangleVertices -> (v0,v1,v2)) = show v0 ++ show v1 ++ show v2

deriving instance Eq Triangle 
deriving instance Ord Triangle 

instance Enum Triangle where
    toEnum n = triangle (toEnum (3-n) :: Vertex)
    fromEnum t = 3 - fromEnum (vertex t) 

instance Bounded Triangle where
    minBound = tABC
    maxBound = tBCD

triangleVertices :: Triangle -> (Vertex, Vertex, Vertex)
triangleVertices t = fromList3 ( filter4 (`isVertexOfTriangle` t) allVertices' )
              

tABC, tABD, tACD, tBCD :: Triangle
tABC = triangle vD
tABD = triangle vC
tACD = triangle vB
tBCD = triangle vA

allTriangles' ::  (Triangle, Triangle, Triangle, Triangle)
allTriangles' =  ( tABC , tABD , tACD , tBCD )

allTriangles ::  [Triangle]
allTriangles =  asList allTriangles'

trianglePrettyColor :: Doc -> Doc
trianglePrettyColor = cyan

instance Pretty Triangle where pretty = trianglePrettyColor . text . show

class MakeTriangle a where
    triangle :: a -> Triangle

-- | Construct the triangle dual to a given vertex
instance MakeTriangle Vertex where
    triangle x = Triangle (BitSet.complement (BitSet.singleton x))

-- | Construct the vertex dual to a given triangle
instance MakeVertex Triangle where
    vertex (Triangle x) = fromList1 (BitSet.toList $ BitSet.complement x)

-- | The Vertices must be distinct (but needn't be ordered)
instance MakeTriangle (Vertex,Vertex,Vertex) where
    triangle (v0,v1,v2) = triangle (BitSet.insert v0 $ BitSet.insert v1 $ BitSet.singleton v2 )

prop_MakeTriangle_VVV ::  Vertex -> Property
prop_MakeTriangle_VVV v0 = forAll (elements vs') $ \v1 -> forAll (elements (vs' \\ [v1])) $ \v2 -> 
                            let v012 = (v0,v1,v2) in asList v012 `setEq` (asList . triangleVertices) (triangle v012)
    where
        vs' = allVertices \\ [v0]


-- | The BitSet must have exactly three elements
instance MakeTriangle (BitSet Vertex) where
    triangle b = assert (BitSet.size b == 3) (Triangle b)





-- | Construct a triangle containing the two edges
instance MakeTriangle (Pair Edge) where
    triangle es@(edgeToBitSet -> e1,edgeToBitSet -> e2) = 
        case mappend e1 e2 of
            e' | BitSet.size e' == 3 -> Triangle e'
               | otherwise -> error ("triangle "++show es)


prop_MakeTriangle_EE ::  Edge -> Edge -> Property
prop_MakeTriangle_EE e1 e2 = (e1 /= e2 && e1 /= oppositeEdge e2) ==> 
                                let
                                   t = triangle (e1,e2)
                                in
                                    (e1 `isEdgeOfTriangle` t) .&. (e2 `isEdgeOfTriangle` t)
                                    


isVertexOfTriangle :: Vertex -> Triangle -> Bool
isVertexOfTriangle v (Triangle x) = BitSet.member v x


isEdgeOfTriangle :: Edge -> Triangle -> Bool
isEdgeOfTriangle (edgeToBitSet -> x) (Triangle x') = BitSet.isSubsetOf x x'

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

-- | An 'OTriangle' with a tetrahedron index attached to it
type OITriangle = I OTriangle

trivialHasTIndexInstance [t|OTriangle|]

instance  (OrderableFace ITriangle OITriangle) where
    type VertexSymGroup ITriangle = S3
    type VertexTuple ITriangle = Triple IVertex
    unpackOrderedFace (viewI -> I i (unpackOrderedFace -> (g,t))) = (g, (./) i t)
    packOrderedFace g = mapI (packOrderedFace g)




instance LeftAction S3 OITriangle where (.*) = defaultLeftActionForOrderedFace

instance OrderableFace Triangle OTriangle where
    type VertexSymGroup Triangle = S3
    type VertexTuple Triangle = Triple Vertex
    unpackOrderedFace (OTriangle g x) = (g,x)
    packOrderedFace = OTriangle


instance LeftAction S3 OTriangle where
    (.*) = defaultLeftActionForOrderedFace



-- | Ordered edges contained in a given facet, in order
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


        caseOrdered w0 w1 w2 = packOrderedFace S3abc $
                case map3 vertexToWord8 (w0,w1,w2) of
                     (0,1,2) -> tABC
                     (0,1,3) -> tABD
                     (0,2,3) -> tACD
                     (1,2,3) -> tBCD
                     _ -> error (unwords ["impossible:","caseOrdered",show w0, show w1, show w2])

-- | Triangles containing a given edge
instance Triangles Edge (Pair Triangle) where
    triangles e = fromList2 ( filter4 (e `isEdgeOfTriangle`) allTriangles' ) 

-- | Edges contained in a given triangle
instance Edges Triangle (Triple Edge) where
    edges t@(Triangle x) = map3 (\v -> bitSetToEdge (BitSet.delete v x)) (v2,v0,v1)
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
instance Triangles Vertex (Triple Triangle) where
    triangles !v = fromList3 (filter4 (isVertexOfTriangle v) allTriangles')

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

vertexByOppositeEdge
  :: (Show a, Vertices a (Triple Vertex)) =>
     Edge -> a -> Vertex
vertexByOppositeEdge e t =
    case filter3 (\v -> not (isVertexOfEdge v e)) (vertices t) of

         [v0] -> v0
         _ -> error ("vertexByOppositeEdge is not defined for args "++show e++", "++show t)



instance Triangles TIndex (Quadruple ITriangle) where
    triangles z = map4 (z ./) allTriangles'

instance Triangles IVertex (Triple ITriangle) where
    triangles (viewI -> I i v) = map3 (i ./) (triangles v) 

instance Triangles IEdge (Pair ITriangle) where
    triangles (viewI -> I i v) = map2 (i ./) (triangles v) 

instance OEdges OTriangle (Triple OEdge) where
    oedges (vertices -> (v0,v1,v2)) = (oedge (v0,v1), oedge (v1,v2), oedge(v2,v0)) 

data VertexIndexInTriangle = VT0 | VT1 | VT2
    deriving(Eq,Show)


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



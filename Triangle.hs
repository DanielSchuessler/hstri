{-# LANGUAGE StandaloneDeriving, NoMonomorphismRestriction, TemplateHaskell, ViewPatterns, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module Triangle where

import Collections
import Control.Exception
import Data.BitSet.Word8 as BitSet
import Element
import Language.Haskell.TH.Syntax as Syntax
import Test.QuickCheck
import Text.PrettyPrint.ANSI.Leijen
import Util
import Vertex
import Data.List as List
import HomogenousTuples
import Edge
import Data.Monoid



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
    triangle es@(Edge e1,Edge e2) = 
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
isEdgeOfTriangle (Edge x) (Triangle x') = BitSet.isSubsetOf x x'

instance Arbitrary Triangle where arbitrary = elements allTriangles
instance Lift Triangle where
    lift (Triangle t) = [| Triangle $(Syntax.lift t) |]
instance Finite Triangle

instance Quote Triangle where
    quotePrec _ t = "t" ++ show t


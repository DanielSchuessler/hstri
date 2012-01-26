{-# LANGUAGE ViewPatterns, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances  #-}
{-# OPTIONS -Wall #-}
module Tetrahedron.NormalArc(
        module Tetrahedron.NormalCorner,

        NormalArc, 
        -- * Construction
        allNormalArcs,
        normalArcsAroundVertex,
        normalArcByTriangleAndVertex,
        normalArcByTriangleAndVertexIndex,
        normalArcByOTriangleAndVertexIndex,
        MakeNormalArc(..),
        NormalArcs(..),
        normalArcList,

        -- * Properties
        normalArcGetTriangle,
        normalArcGetVertex,
        normalArcGetParallelEdge,
        normalArcGetVertexIndex,
        normalArcGetAngle,

    )


    where

import Tetrahedron
import Control.Exception
import Data.Maybe
import Element
import HomogenousTuples
import Language.Haskell.TH.Syntax
import Tetrahedron.NormalCorner
import PrettyUtil
import Quote
import Test.QuickCheck
import Util

data NormalArc = NormalArc !Triangle !Vertex  -- Invariant: The 'Vertex' is contained in the 'Triangle'
    deriving (Eq,Ord)

instance Show NormalArc where
    showsPrec = prettyShowsPrec 

instance NormalCorners NormalArc (Pair NormalCorner) where
    normalCorners (NormalArc t v) =  
            fromList2
          . fmap normalCorner
          . filter3 (v `isSubface`)
          . edges
          $ t





class MakeNormalArc a where
    normalArc :: a -> NormalArc

-- | Construct a normal arc by the triangle that contains it and the vertex (of that triangle) 
-- dual to the normal arc (i.e., the vertex which the normal arc separates from the other two vertices of the triangle)
instance MakeNormalArc (Triangle,Vertex) where
    normalArc (f,v) = normalArcByTriangleAndVertex f v
    
    
-- | Construct a normal arc by the triangle that contains it and the vertex (of that triangle) 
-- dual to the normal arc (i.e., the vertex which the normal arc separates from the other two vertices of the triangle)
normalArcByTriangleAndVertex :: Triangle -> Vertex -> NormalArc
normalArcByTriangleAndVertex f v = assert (isSubface v f) 
                        (NormalArc f v)


-- | Construct a normal arc by the two edges it intersects
instance MakeNormalArc (Edge,Edge) where
    normalArc es = normalArc (triangle es, vertex es) 

-- | Construct a normal arc by its normal corners
instance MakeNormalArc (NormalCorner,NormalCorner) where
    normalArc = normalArc . map2 normalCornerGetContainingEdge 

normalArcList ::  NormalArcs a normalArcTuple => a -> [Element normalArcTuple]
normalArcList = asList . normalArcs


-- | Gets the triangle containing the given normal arc
normalArcGetTriangle ::  NormalArc -> Triangle
normalArcGetTriangle (NormalArc f _) = f

-- | Gets the vertex separated by the normal arc from the other two vertices in the 'normalArcGetTriangle'
normalArcGetVertex ::  NormalArc -> Vertex
normalArcGetVertex (NormalArc _ v) = v


-- | The index of the enclosed vertex in its triangle
normalArcGetVertexIndex :: NormalArc -> Index3
normalArcGetVertexIndex (NormalArc t v) = 
    fromMaybe (assert False undefined)
    (triangleGetIndexOf t v)

normalArcByTriangleAndVertexIndex :: Triangle -> Index3 -> NormalArc
normalArcByTriangleAndVertexIndex t i = 
    normalArcByTriangleAndVertex t (triangleGetVertexAt t i) 

normalArcByOTriangleAndVertexIndex :: OTriangle -> Index3 -> NormalArc
normalArcByOTriangleAndVertexIndex t i = 
    normalArcByTriangleAndVertex (forgetVertexOrder t) (triangleGetVertexAt t i) 

-- | Gets the edge which is contained in the 'normalArcGetTriangle' of the 
-- given normal arc, but disjoint from the normal arc
normalArcGetParallelEdge ::  NormalArc -> Edge
normalArcGetParallelEdge nat = edgeByOppositeVertexAndTriangle (normalArcGetVertex nat) (normalArcGetTriangle nat)


instance Enum NormalArc where
    fromEnum (NormalArc f v) = 3 * fromEnum f + indexInTriple v (vertices f) 
    toEnum n = assert (n >= 0) $
               assert (n < 12) $
                   let 
                        (d,m) = divMod n 3
                        f :: Triangle
                        f = toEnum d
                   in normalArc (f, vertexList f !! m)

instance Bounded NormalArc where
    minBound = normalArc (f, minimum . vertexList $ f)
        where
            f :: Triangle
            f = minBound

    maxBound = normalArc (f , maximum . vertexList $ f)
        where
            f :: Triangle
            f = maxBound


allNormalArcs :: [NormalArc]
allNormalArcs = [minBound .. maxBound]


instance Pretty NormalArc where 
    pretty = green . text . quote
--     pretty na = green (lbrace <> text "Arc dual to" <+> 
--                         pretty (normalArcGetVertex na) <+> text "in" <+> 
--                         pretty (normalArcGetTriangle na) <>
--                             rbrace)


class AsList normalArcTuple => NormalArcs a normalArcTuple | a -> normalArcTuple where
    normalArcs :: a -> normalArcTuple

instance Arbitrary NormalArc where
    arbitrary = elements allNormalArcs

instance NormalArcs Triangle (Triple NormalArc) where
    normalArcs (normalCorners -> (x0,x1,x2)) = (normalArc (x2,x0), normalArc (x0,x1), normalArc (x1,x2))

instance NormalArcs OTriangle (Triple NormalArc) where
    normalArcs (normalCorners -> (x0,x1,x2)) = (normalArc (x2,x0), normalArc (x0,x1), normalArc (x1,x2))







instance IsSubface NormalCorner NormalArc where
    isSubface c (normalCorners -> (c0,c1)) = c==c0 || c==c1

instance IsSubface NormalArc OTriangle where
    isSubface na t = isSubface na (forgetVertexOrder t)

instance IsSubface NormalArc Triangle where
    isSubface nat t = normalArcGetTriangle nat == t

-- | The middle returned vertex is the 'normalArcGetVertex' of the 'NormalArc'; the other two complete the 'normalArcGetTriangle'
normalArcGetAngle :: NormalArc -> (Vertex, Vertex, Vertex)
normalArcGetAngle na = (v0,v,v1)
    where
        v = normalArcGetVertex na
        (v0,v1) = vertices (edgeByOppositeVertexAndTriangle v (normalArcGetTriangle na))

        

instance Quote NormalArc where
    quote (normalArcGetAngle -> (v0,v,v1)) =
        "na" ++ concatMap show [v0',v,v1']
      where
        (v0',v1') = sort2 (v0,v1)


instance Lift NormalArc where
    lift (NormalArc x y) = [| NormalArc x y |]

instance MakeVertex NormalArc where
    vertex = normalArcGetVertex

normalArcsAroundVertex :: Vertex -> Triple NormalArc
normalArcsAroundVertex v = 
    map3 (`normalArcByTriangleAndVertex` v) (trianglesContainingVertex v)


instance Finite NormalArc

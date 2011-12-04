{-# LANGUAGE ViewPatterns, TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances  #-}
{-# OPTIONS -Wall #-}
module NormalArc(
        module NormalCorner,

        NormalArc, normalArcGetTriangle, normalArcGetVertex, allNormalArcs,
            normalArcGetOppositeEdge, normalArcGetVertexIndex, normalArcByTriangleAndVertexIndex,
            normalArcByTriangleAndVertex,
        MakeNormalArc(..),
        NormalArcs(..),
        normalArcList,
        qc_NormalArc
    )


    where

import NormalCorner
import AbstractTetrahedron
import HomogenousTuples
import Control.Exception
import Element
import Test.QuickCheck
import Test.QuickCheck.All
import PrettyUtil

data NormalArc = NormalArc Triangle Vertex  -- Invariant: The 'Vertex' is contained in the 'Triangle'
    deriving (Eq,Ord)

instance Show NormalArc where
    showsPrec = prettyShowsPrec 

instance NormalCorners NormalArc (Pair NormalCorner) where
    normalCorners (NormalArc t v) =  
        (   fromList2
          . fmap normalCorner
          . filter3 (v `isSubface`)) (edges t)



prop_NormalCornersOfNormalArc_distinct :: NormalArc -> Bool
prop_NormalCornersOfNormalArc_distinct nat = let (c1,c2) = normalCorners nat in c1 /= c2


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


normalArcGetVertexIndex :: NormalArc -> Maybe VertexIndexInTriangle
normalArcGetVertexIndex (NormalArc t v) = triangleGetIndexOf t v

normalArcByTriangleAndVertexIndex :: Triangle -> VertexIndexInTriangle -> NormalArc
normalArcByTriangleAndVertexIndex t i = NormalArc t (triangleGetVertexAt t i) 

-- | Gets the edge which is contained in the 'normalArcGetTriangle' of the 
-- given normal arc, but disjoint from the normal arc
normalArcGetOppositeEdge ::  NormalArc -> Edge
normalArcGetOppositeEdge nat = edgeByOppositeVertexAndTriangle (normalArcGetVertex nat) (normalArcGetTriangle nat)


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
    pretty na = green (lbrace <> text "Arc dual to" <+> 
                        pretty (normalArcGetVertex na) <+> text "in" <+> 
                        pretty (normalArcGetTriangle na) <>
                            rbrace)


class AsList normalArcTuple => NormalArcs a normalArcTuple | a -> normalArcTuple where
    normalArcs :: a -> normalArcTuple

instance Arbitrary NormalArc where
    arbitrary = elements [ minBound .. maxBound ]

instance NormalArcs Triangle (Triple NormalArc) where
    normalArcs (normalCorners -> (x0,x1,x2)) = (normalArc (x2,x0), normalArc (x0,x1), normalArc (x1,x2))

instance NormalArcs OTriangle (Triple NormalArc) where
    normalArcs (normalCorners -> (x0,x1,x2)) = (normalArc (x2,x0), normalArc (x0,x1), normalArc (x1,x2))

instance IsSubface NormalArc Triangle where
    isSubface nat t = normalArcGetTriangle nat == t

prop_Triangle_NormalArcs_correct :: Triangle -> Bool
prop_Triangle_NormalArcs_correct t = all3 (`isSubface` t) (normalArcs t) 

prop_Triangle_NormalArcs_complete :: NormalArc -> Triangle -> Property
prop_Triangle_NormalArcs_complete nat t = 
    isSubface nat t ==> 
        any3 (==nat) (normalArcs t) 


qc_NormalArc :: IO Bool
qc_NormalArc = $(quickCheckAll)

instance IsSubface NormalArc OTriangle where
    isSubface na t = isSubface na (forgetVertexOrder t)

prop_normalArcByNormalCorners :: NormalArc -> Property
prop_normalArcByNormalCorners na = 
        na == normalArc (nc1,nc2)
        .&.
        na == normalArc (nc2,nc1)
    where
        (nc1,nc2) = normalCorners na 

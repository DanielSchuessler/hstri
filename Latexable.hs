{-# LANGUAGE TupleSections, NoMonomorphismRestriction, ViewPatterns, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS -Wall #-}
module Latexable(Latex,Latexable(..),listToLatex,latexSet,mathmode
    ,latexifyStandardMatchingEquations
    ,latexifyQMatchingEquations
    
    ) where

import Control.Arrow(first)
import Data.Function
import Data.List(intercalate,sort,sortBy,groupBy)
import Data.Monoid
import Data.Ord
import Data.Semigroup
import Element
import INormalDisc
import QuadCoordinates
import StandardCoordinates
import Triangulation
import TriangulationCxtObject
import Data.Maybe(mapMaybe)

type Latex = String

class Latexable a where
    toLatex :: a -> Latex


op1 :: String -> Latex -> Latex
op1 f x = "\\"<>f<>"{"<>x<>"}"
op2 :: String -> Latex -> Latex -> Latex
op2 f x y = op1 f (x<>"}{"<>y)


instance Latexable Gluing where
    toLatex (tri,otri) = op2 "AffIso" (toLatex tri) (toLatex otri)

instance Latexable ITriangle where
    toLatex = toLatex . toOrderedFace

instance Latexable IEdge where
    toLatex = toLatex . toOrderedFace

instance Latexable OITriangle where
    toLatex (viewI -> I ti (vertices -> (v0,v1,v2))) = 
        op1 "facet" (show v0 <> show v1 <> show v2 <> "_" <> show ti)

instance Latexable OIEdge where
    toLatex (viewI -> I ti (vertices -> (v0,v1))) = 
        op1 "edge" (show v0 <> show v1 <> "_" <> show ti)

instance Latexable IVertex where
    toLatex (viewI -> I ti v0) =
        op1 "vertex" (show v0 <> "_" <> show ti)

instance Latexable TIndex where
    toLatex = show . toInteger

listToLatex :: (AsList a, Latexable (Element a)) => a -> Latex
listToLatex = intercalate ", " . fmap toLatex . asList

latexSet :: (AsList a, Latexable (Element a)) => a -> Latex
latexSet xs = "\\left\\{" <> listToLatex xs <> "\\right\\}" 

mathmode :: Latex -> Latex
mathmode x = "$" <> x <> "$"

instance Latexable Triangulation where
    toLatex tr =
        unlines $

            begin "matrix"
            :
            fmap (\gs -> intercalate ", & " (fmap toLatex (sort gs)) <> "\\\\" ) 
            gss
            <>
            [end "matrix"]
     where
            gss = groupBy ((==) `on` (getTIndex . fst)) . sort . tGluingsIrredundant $ tr

instance Latexable a => Latexable (CanonOrdered a) where
    toLatex = toLatex . unCanonOrdered

instance Latexable INormalTri where
    toLatex tri = op1 "Tri" (toLatex (iNormalTriGetVertex tri))

instance Latexable INormalQuad where
    toLatex tri = op1 "Quad" (toLatex (fst (iNormalQuadGetDisjointEdges tri)))

instance Latexable INormalDisc where
    toLatex = eitherIND toLatex toLatex

instance Latexable Integer where
    toLatex = show

-- latexIntegral :: Integral a => a -> Latex
-- latexIntegral = toLatex . toInteger

begin :: Latex -> Latex
begin = op1 "begin"
end :: Latex -> Latex
end = op1 "end"

-- | Left: verbatim row (not followed by @\\\\@)
-- Right: cells (@&@ and @\\\\@ added) 
tabularLike :: String -> [Latex] -> Latex -> String
tabularLike env colSpecs body = 
    unlines
        [
            begin env <> "{" <> concat colSpecs <> "}"
          , body
          , end env
        ]


amps :: [Latex] -> Latex
amps = intercalate "&"
slashes :: [Latex] -> Latex
slashes [] = ""
slashes xs = foldr1 (\\) xs

(\\) :: Latex -> Latex -> Latex
x \\ y = x <> "\\\\\n" <> y

briefDiscType :: MakeINormalDisc a => a -> Latex
briefDiscType =
    eitherIND
        (toLatex . iNormalTriGetVertex)
        (toLatex . fst . iNormalQuadGetDisjointEdges)
    . iNormalDisc

zeroAsBlank :: (Num a, Latexable a) => a -> Latex
zeroAsBlank x = if x == 0
                          then mempty
                          else mathmode (toLatex x)

tabular :: [Latex] -> Latex -> String
tabular = tabularLike "tabular" 

small :: Latex -> Latex
small x = "{\\small"<> x <> "}"

meHeader :: MakeINormalDisc a => [a] -> Latex
meHeader discs = amps (fmap headerCell discs)
    where
                headerCell = small . mathmode . briefDiscType 

latexifyStandardMatchingEquations :: Triangulation -> Latex
latexifyStandardMatchingEquations tr =
    let
        mes = 
            sortBy (comparing (fmap (/= (0::Integer)) . fst)) .
            fmap (first (stc_toDenseList tr)) .
            matchingEquationsWithReasons $ tr

        colSpecs = replicate (fi $ tNumberOfNormalDiscTypes tr) "r"

        body = meHeader (tINormalDiscs tr) \\ 
               ("\\hline" <> slashes (fmap mkRow mes))


        mkRow (sc,_) = amps (fmap zeroAsBlank sc)
    in
        tabular colSpecs body
            
latexifyQMatchingEquations :: Triangulation -> Latex
latexifyQMatchingEquations tr =
    let
        mes = 
                sortBy (comparing (fmap (/= (0::Integer)) . fst))
            .   mapMaybe 
                    (\e -> fmap ((,e) . quad_toDenseList tr) . qMatchingEquation $ e) 
            .   edges $ tr

        colSpecs = "l" : "|" : replicate (fi $ tNumberOfNormalQuadTypes tr) "r"

        body = ("Kante&"<>meHeader (tINormalQuads tr)) \\ 
               ("\\hline " <> slashes (fmap mkRow mes))


        mkRow (me,e) = mathmode (toLatex e) <> "&" <> amps (fmap zeroAsBlank me)
    in
        tabular colSpecs body

toLatexT :: (IsEquivalenceClass (T a), Latexable (Element (T a))) =>
     T a -> [Char]
toLatexT x = "p("<>toLatex (canonicalRep x)<>")"

instance Latexable TTriangle where toLatex = toLatexT
instance Latexable TEdge where toLatex = toLatexT
instance Latexable TVertex where toLatex = toLatexT

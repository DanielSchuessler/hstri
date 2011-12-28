{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Latexable(Latex,Latexable(..),listToLatex,latexSet,mathmode,latexifyStandardMatchingEquations) where

import Data.List as List
import Data.Function
import Data.Monoid
import Element
import Triangulation
import StandardCoordinates
import INormalDisc
import Data.Semigroup
import Data.Ord
import Control.Arrow(first)

type Latex = String

class Latexable a where
    toLatex :: a -> Latex


op1 :: String -> Latex -> Latex
op1 f x = "\\"<>f<>"{"<>x<>"}"
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
            gss = List.groupBy ((==) `on` (getTIndex . fst)) . sort . tGluingsIrredundant $ tr

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

latexIntegral = toLatex . toInteger

begin = op1 "begin"
end = op1 "end"

-- | Left: verbatim row (not followed by @\\\\@)
-- Right: cells (@&@ and @\\\\@ added) 
tabularLike :: String -> [Latex] -> [Either Latex [Latex]] -> String
tabularLike env colSpecs rows_ = 
    unlines
        (
            (begin env <> "{" <> concat colSpecs <> "}")
          : fmap hr rows_
          <> [end env]
        )

  where
    hr (Right row_) = intercalate "&" row_ ++ "\\\\"
    hr (Left row_) = row_

zeroAsBlank :: (Num a, Latexable a) => a -> Latex
zeroAsBlank 0 = mempty
zeroAsBlank n = toLatex n

briefDiscType :: INormalDisc -> Latex
briefDiscType =
    eitherIND
        (toLatex . iNormalTriGetVertex)
        (toLatex . fst . iNormalQuadGetDisjointEdges)


latexifyStandardMatchingEquations :: Triangulation -> Latex
latexifyStandardMatchingEquations tr =
    let
        mes = 
            sortBy (comparing (fmap (/= 0) . fst)) .
            fmap (first (stc_toDenseList tr)) .
            matchingEquationsWithReasons $ tr

        colSpecs = replicate (fi $ tNumberOfNormalDiscTypes tr) "r"
        rows_ = header ++ fmap mkRow mes

        header = [ 
                    Right $ fmap headerCell (tINormalDiscs tr),
                    Left "\\hline"
                 ]
            where
                --headerCell x = "{\\tiny"<>mathmode(toLatex x)<>"}"
                headerCell x = 
                    "{\\small"<> (mathmode . briefDiscType) x <> "}"

        mkRow (sc,mer) = Right $
            fmap 
                (\x -> if x == 0
                          then mempty
                          else mathmode (toLatex (x :: Integer)))

                sc
    in
        tabularLike "tabular" colSpecs rows_
            


{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Latexable where

import Data.List as List
import Triangulation
import Data.Function
import Data.Monoid


class Latexable a where
    toLatex :: a -> String


instance Latexable Triangulation where
    toLatex tr =
        unlines $

            "\\begin{matrix}"
            :
            fmap (\gs -> intercalate ", & " (fmap toLatex (sort gs)) ++ "\\\\" ) 
            gss
            ++
            ["\\end{matrix}"]
     where
            gss = List.groupBy ((==) `on` (getTIndex . fst)) . sort . tGluingsIrredundant $ tr



instance Latexable Gluing where
    toLatex (tri,otri) = "\\left("++toLatex tri++","++toLatex otri++"\\right)"

instance Latexable ITriangle where
    toLatex = toLatex . packOrderedFace mempty

instance Latexable IEdge where
    toLatex = toLatex . packOrderedFace mempty

instance Latexable OITriangle where
    toLatex (viewI -> I ti (vertices -> (v0,v1,v2))) = 
        show v0 ++ show v1 ++ show v2 ++ "_" ++ show ti

instance Latexable OIEdge where
    toLatex (viewI -> I ti (vertices -> (v0,v1))) = 
        show v0 ++ show v1 ++ "_" ++ show ti

instance Latexable IVertex where
    toLatex (viewI -> I ti v0) =
        show v0 ++ "_" ++ show ti


listToLatex :: Latexable a => [a] -> String
listToLatex = intercalate ", " . fmap toLatex

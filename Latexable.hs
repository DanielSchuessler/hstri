{-# LANGUAGE TypeFamilies, QuasiQuotes, TupleSections, NoMonomorphismRestriction, ViewPatterns, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
-- {-# OPTIONS -Wall #-}
module Latexable(
     Latex,Latexable(..),listToLatex,latexSet,mathmode
    ,latexifyStandardMatchingEquations
    ,latexifyQMatchingEquations
    ,latexTwoRows
    ,op1,op2
    ,textcolor
    ,verboseTri,verboseQuad,verboseArc
    ,quad_latex
    
    -- * Program invocation
    ,runPdfLatex,runPdfLatexSilent,runOkularAsync
    ) where

import Control.Applicative
import Control.Arrow(first)
import Data.Function
import Data.List(intercalate,sort,sortBy,groupBy)
import Data.Maybe(mapMaybe)
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.Semigroup
import Element
import INormalDisc
import MathUtil
import QuadCoordinates
import StandardCoordinates
import Triangulation
import Triangulation.CanonOrdered
import TriangulationCxtObject
import ZeroDefaultMap
import Util
import Data.String.Interpolation
import System.Process
import qualified Data.ByteString.Lazy as B
import System.IO as IO
import Control.Concurrent

type Latex = String

class Latexable a where
    toLatex :: a -> Latex


op1 :: Latexable a => String -> a -> Latex
op1 f x = "\\"<>f<>"{"<>toLatex x<>"}"
op2 :: (Latexable a, Latexable a1) => String -> a -> a1 -> Latex
op2 f x y = op1 f (toLatex x<>"}{"<>toLatex y)

instance Latexable Latex where
    toLatex = id

instance Latexable Gluing where
    toLatex (tri,otri) = op2 "AffIso" (toLatex tri) (toLatex otri)

instance Latexable ITriangle where
    toLatex = toLatex . toOrderedFace

instance Latexable IEdge where
    toLatex = toLatex . toOrderedFace


latexTriangleBrief
  :: (Show a1,
      Show a2,
      Show a3,
      Vertices a,
      HasTIndex ia a,
      Verts a ~ (a1, a2, a3)) =>
     ia -> String
latexTriangleBrief (viewI -> I ti (vertices -> (v0,v1,v2))) = 
    show v0 <> show v1 <> show v2 <> "_" <> show ti

instance Latexable OITriangle where
    toLatex = latexTriangleBrief


latexEdgeBrief
  :: (Show a1,
      Show a2,
      Vertices a,
      HasTIndex ia a,
      Verts a ~ (a1, a2)) =>
     ia -> String
latexEdgeBrief (viewI -> I ti (vertices -> (v0,v1))) = (show v0 <> show v1 <> "_" <> show ti)

instance Latexable OIEdge where
    toLatex = latexEdgeBrief

latexVertexBrief :: (Show a, HasTIndex ia a) => ia -> String
latexVertexBrief (viewI -> I ti v0) = (show v0 <> "_" <> show ti)

operatorname :: String -> Latex
operatorname = op1 "operatorname"

instance Latexable IVertex where
    toLatex = latexVertexBrief 

instance Latexable TIndex where
    toLatex = show . toInteger

listToLatex :: (AsList a, Latexable (Element a)) => a -> Latex
listToLatex = intercalate ", " . fmap toLatex . asList

latexSet :: (AsList a, Latexable (Element a)) => a -> Latex
latexSet xs = "\\left\\{" <> listToLatex xs <> "\\right\\}" 

mathmode :: Latexable a => a -> [Char]
mathmode x = "$" <> toLatex x <> "$"

instance Latexable Triangulation where
    toLatex tr =

            latexEnvNL "matrix"
            (
            unlines $
            fmap (\gs -> intercalate ", & " (fmap toLatex (sort gs)) <> "\\\\" ) 
            gss
            )
     where
            gss = groupBy ((==) `on` (getTIndex . fst)) . sort . tGluingsIrredundant $ tr

instance Latexable a => Latexable (CanonOrdered a) where
    toLatex = toLatex . unCanonOrdered

tuple :: Latexable a => [a] -> Latex
tuple xs = "("<>intercalate "," (toLatex <$> xs)<>")"


verboseTri :: INormalTri -> Latex
verboseTri tri = operatorname "Tri" <> tuple [ iNormalTriGetVertex tri ]

verboseQuad :: INormalQuad -> Latex
verboseQuad x = operatorname "Quad" <> tuple [ fst (iNormalQuadGetDisjointEdges x) ]

instance Latexable INormalTri where
    toLatex = toLatex . iNormalTriGetVertex 

instance Latexable INormalQuad where
    toLatex = toLatex . fst . iNormalQuadGetDisjointEdges

instance Latexable INormalDisc where
    toLatex = eitherIND toLatex toLatex


verboseArc :: INormalArc -> Latex
verboseArc ina = operatorname "Arc" <> tuple [
                        latexTriangleBrief $ iNormalArcGetTriangle ina 
                     ,  toLatex (iNormalArcGetVertexIndex ina)
                    ]

instance Latexable INormalArc where
    toLatex ina = 
                        latexTriangleBrief (iNormalArcGetTriangle ina) 
                        <>
                            ","
                        <>
                        toLatex (iNormalArcGetVertexIndex ina)

instance Latexable TNormalArc where
    toLatex tna = case normalArcPreimage tna of
                     BoundaryNormalArc ina -> "\\{"<>toLatex ina<>"\\}" 
                     InnerNormalArc ina1 ina2 -> toLatex (InnNA ina1 ina2)
                     
instance Latexable InnNA where
    toLatex (InnNA ina1 ina2) = latexTwoRows (toLatex ina1 <> sim) ina2

sim :: Latex
sim = "\\sim"

instance Latexable Integer where toLatex = show
instance Latexable Int where toLatex = show
instance Latexable VertexIndexInTriangle where toLatex = show . fromEnum


-- latexIntegral :: Integral a => a -> Latex
-- latexIntegral = toLatex . toInteger

begin :: Latex -> Latex
begin = op1 "begin"
end :: Latex -> Latex
end = op1 "end"

latexEnv :: Latex -> Latex -> Latex
latexEnv x body = begin x <> body <> end x

latexEnvNL :: Latex -> String -> String
latexEnvNL x body = unlines [ begin x, body, end x ]

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
            fmap (first (ns_toDenseList tr)) .
            matchingEquationsWithReasons $ tr

        colSpecs = replicate (tNumberOfNormalDiscTypes tr) "r"

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

        colSpecs = "l" : "|" : replicate (tNumberOfNormalQuadTypes tr) "r"

        body = ("Kante&"<>meHeader (tINormalQuads tr)) \\ 
               ("\\hline " <> slashes (fmap mkRow mes))


        mkRow (me,e) = mathmode (toLatex e) <> "&" <> amps (fmap zeroAsBlank me)
    in
        tabular colSpecs body

toLatexT :: (IsEquivalenceClass (T a), Latexable (Element (T a))) =>
     T a -> Latex
toLatexT x = "p("<>toLatex (canonicalRep x)<>")"

instance Latexable TTriangle where toLatex = toLatexT
instance Latexable TEdge where toLatex = toLatexT
instance Latexable TVertex where toLatex = toLatexT

latexTwoRows :: (Latexable a, Latexable a1) => a -> a1 -> Latex
latexTwoRows x y =
       matrix $
       toLatex x
    <> "\\\\"
    <> toLatex y

textcolor :: (Latexable a, Latexable a1) => a -> a1 -> Latex
textcolor = op2 "textcolor"


matrix :: Latex -> Latex
matrix = latexEnv "matrix"

instance (Integral i, Latexable i) => Latexable (Ratio i) where

    toLatex x 
        | Just i <- ratioToIntegral x = toLatex i 
        | otherwise = (op2 "frac" <$> numerator <*> denominator) x


instance (Latexable k, Latexable r, Num r, Ord r) => Latexable (ZeroDefaultMap k r) where

    toLatex = zdm_showWith id "" (++) toLatex toLatex 

instance Latexable Variable where toLatex = variableName


pdfLatexOptions texfile = ["-interaction","nonstopmode","-file-line-error" 
                                        ,"-halt-on-error"
                                        ,"-output-directory","/tmp",texfile] 

runPdfLatex :: FilePath -> IO ()
runPdfLatex texfile = rawSystemS "pdflatex" (pdfLatexOptions texfile) 

runPdfLatexSilent texfile = do 
    (Nothing,Just so,Nothing,ph) <- 
        createProcess   
            (proc "pdflatex" (pdfLatexOptions texfile))
                { std_out = CreatePipe }

    forkIO (B.writeFile "/tmp/pdflatex-drivel" =<< B.hGetContents so)

    waitForProcess ph



quad_latex
  :: (Num a, Latexable a) =>
     Triangulation -> QuadCoordinates a -> Latex
quad_latex tr qc = 
    case quad_toDenseList tr qc of
         lst -> latexEnv "pmatrix" (amps . fmap toLatex $ lst)


runOkularAsync pdffile =
                 rawSystemS "zsh" ["-c", "okular $1 &", "arg0", pdffile]


{-# LANGUAGE OverlappingInstances, NamedFieldPuns, TemplateHaskell, TransformListComp, ScopedTypeVariables, TypeFamilies, QuasiQuotes, TupleSections, NoMonomorphismRestriction, ViewPatterns, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-unused-binds -fwarn-missing-local-sigs #-}
module Latexable(
     Latex,Latexable(..),listToLatex,latexSet,mathmode
    ,latexifyStandardMatchingEquations
    ,latexifyQMatchingEquations
    ,latexifyEns
    ,latexTwoRows
    ,op1,op2
    ,textcolor
    ,verboseTri,verboseQuad,verboseArc
    ,briefDiscType
    ,veryBriefDiscType,veryBriefEdge
    ,quad_latex
    ,slashes,amps,newcommand,tabular,array,latexEnv,latexEnvNL,(&),unlineses,align,plus,hspace,vspace
    ,nl
    ,safeUnlines
    
    -- * Program invocation
    ,runPdfLatex,runPdfLatexSilent,runPdfLatexSilentS,runOkularAsync
    ) where

import Control.Applicative
import Control.Arrow(first)
import Control.Concurrent
import Data.Function
import Data.List(intercalate,sort,sortBy,groupBy)
import Data.Maybe(mapMaybe)
import Data.Ord
import Data.Ratio
import Data.Semigroup
import Element
import Math.SparseVector
import QuadCoordinates
import QuadCoordinates.Class
import StandardCoordinates
import System.IO as IO
import System.Process(createProcess,proc,std_out,StdStream(CreatePipe),waitForProcess)
import Triangulation
import Triangulation.CanonOrdered
import Triangulation.InnerNormalArc
import TriangulationCxtObject
import Util
import qualified Data.ByteString.Lazy as B
import Triangulation.PreTriangulation
import Control.Monad
import System.Exit
import Triangulation.AbstractNeighborhood
import VerboseDD

type Latex = String

class Latexable a where
    toLatex :: a -> Latex

nl :: Latex
nl = "%\n"

safeUnlines :: [Latex] -> Latex
safeUnlines = concatMap (++nl)

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

tuple' :: Latexable a => [a] -> [Char]
tuple' xs = "\\left("<>intercalate "," (toLatex <$> xs)<>"\\right)"

verboseTri :: INormalTri -> Latex
verboseTri tri = op1 "Tri" (iNormalTriGetVertex tri)

verboseQuad :: INormalQuad -> Latex
verboseQuad x = op1 "Quad" ( fst (iNormalQuadGetDisjointEdges x) )

instance Latexable INormalTri where
    toLatex = 
        verboseTri
--         toLatex . iNormalTriGetVertex 

instance Latexable INormalQuad where
    toLatex = 
        verboseQuad
--         toLatex . fst . iNormalQuadGetDisjointEdges

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
instance Latexable Index3 where toLatex = show . fromEnum


-- latexIntegral :: Integral a => a -> Latex
-- latexIntegral = toLatex . toInteger

begin :: Latex -> Latex
begin = op1 "begin"
end :: Latex -> Latex
end = op1 "end"

latexEnv :: Latex -> Latex -> Latex
latexEnv x body = begin x <> "%\n" <> body <> "%\n" <> end x

latexEnvNL :: Latex -> String -> String
latexEnvNL x body = unlines [ begin x, body, end x ]

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


veryBriefDiscType
  :: (MakeINormalDisc a, Latexable a1) => a1 -> a -> Latex
veryBriefDiscType space =
    eitherIND
        (toLatex . iNormalTriGetVertex)
        (veryBriefEdge space . fst . iNormalQuadGetDisjointEdges)
    . iNormalDisc

veryBriefEdge
  :: (Show a1,
      Show a3,
      Vertices a,
      HasTIndex ia a,
      Latexable a2,
      Verts a ~ (a1, a3)) =>
     a2 -> ia -> [Char]
veryBriefEdge space (viewI -> I i (vertices -> (v0,v1))) =
    show v0 ++ hspace space ++ show v1 ++ "_{"++show i++"}" 


zeroAsBlank :: (Eq a, Num a, Latexable a) => a -> Latex
zeroAsBlank x = if x == 0
                          then mempty
                          else mathmode (toLatex x)

tabular :: [Latex] -> Latex -> String
tabular = tabularLike "tabular" 

array :: [Latex] -> Latex -> String
array = tabularLike "array" 



small :: Latex -> Latex
small x = "{\\small"<> x <> "}"

matchingEquationsHeader
  :: Latexable b => (Latex -> Latex) -> (a -> b) -> [a] -> Latex
matchingEquationsHeader _size discTypeToLatex discs = amps (fmap headerCell discs)
    where
                headerCell = _size . mathmode . discTypeToLatex 

latexifyStandardMatchingEquations :: Triangulation -> Latex
latexifyStandardMatchingEquations tr =
    let
        mes = 
            sortBy (comparing (fmap (/= (0::Integer)) . fst)) .
            fmap (first (ns_toDenseList tr)) .
            matchingEquationsWithReasons $ tr

        colSpecs = replicate (tNumberOfNormalDiscTypes tr) "r"

        body = matchingEquationsHeader small briefDiscType (tINormalDiscs tr) \\ 
               ("\\hline" <> slashes (fmap mkRow mes))


        mkRow (sc,_) = amps (fmap zeroAsBlank sc)
    in
        tabular colSpecs body

hspace,vspace :: Latexable a => a -> Latex
hspace = op1 "hspace"

vspace = op1 "vspace"

            
latexifyQMatchingEquations
  :: String -> String -> Triangulation -> String
latexifyQMatchingEquations columnSep hspaceAfterNumber tr =
    let
        mes = 
                sortBy (comparing (fmap (/= (0::Integer)) . fst))
            .   mapMaybe 
                    (\e -> fmap ((,e) . quad_toDenseList tr) . qMatchingEquation $ e) 
            .   edges $ tr

        colSpecs = "l" : "|" : replicate 
                                    (tNumberOfNormalQuadTypes tr) 
                                    ("r@{\\hspace{"++columnSep++"}}")

        body = ("Kante&"<>
                matchingEquationsHeader 
                        (id)
                        verboseQuad 
                        (tINormalQuads tr)) \\ 
               ("\\hline\n" <> slashes (fmap mkRow mes))


        mkRow (me,e) = mathmode (toLatex e) <> "&" <> amps (fmap mkCell me)

        mkCell 0 = mempty
        mkCell i = mathmode (toLatex i <> hspace hspaceAfterNumber)
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

instance (Eq i, Integral i, Latexable i) => Latexable (Ratio i) where

    toLatex x =
        case (numerator x, denominator x) of
             (n,1) -> toLatex n
             (n,d) | x >= 0    -> op2 "frac" n d
                   | otherwise -> "-"++op2 "frac" (-n) d


plus :: (Num a, Ord a, Latexable a) => a -> Latex
plus y = 
        if y < 0
           then " - " ++ toLatex (-y)
           else " + " ++ toLatex y


instance (Latexable k, Latexable r, Num r, Ord r) => Latexable (SparseVector k r) where

    toLatex = sparse_showWith id "" (++) toLatex toLatex 

instance Latexable Variable where toLatex = variableName


pdfLatexOptions :: FilePath -> [[Char]]
pdfLatexOptions (texfile :: FilePath) = ["-interaction","nonstopmode","-file-line-error" 
                                        ,"-halt-on-error"
                                        ,"-output-directory","/tmp",texfile] 

runPdfLatex :: FilePath -> IO ()
runPdfLatex texfile = rawSystemS "pdflatex" (pdfLatexOptions texfile) 

pdflatexDrivelFile :: FilePath
pdflatexDrivelFile = "/tmp/pdflatex-drivel"

runPdfLatexSilent :: FilePath -> IO ExitCode
runPdfLatexSilent (texfile :: FilePath) = do 
    (Nothing,Just so,Nothing,ph) <- 
        createProcess   
            (proc "pdflatex" (pdfLatexOptions texfile))
                { std_out = CreatePipe }

    _ <- forkIO (B.writeFile pdflatexDrivelFile =<< B.hGetContents so)

    waitForProcess ph

-- | Like 'runPdfLatexSilent, but must succeed.
runPdfLatexSilentS :: FilePath -> IO ()
runPdfLatexSilentS texfile = do 
    ec <- runPdfLatexSilent texfile
    unless (ec==ExitSuccess) $ do
        putStrLn ("Proceeding to dump "++pdflatexDrivelFile++" ...") 
        rawSystemS "cat" [pdflatexDrivelFile]
        (error ("pdflatex failed with "++show ec))



quad_latex
  :: (PreTriangulation tr,
      QuadCoords q a,
      Latexable a) =>
     tr -> q -> Latex
quad_latex tr qc = 
    case quad_toDenseList tr qc of
         lst -> latexEnv "pmatrix" (amps . fmap toLatex $ lst)


runOkularAsync :: String -> IO ()
runOkularAsync pdffile = rawSystemAsyncS "okular" [pdffile]

braces :: [Char] -> [Char]
braces x = "{"++x++"}"



instance Latexable IEdgeNeighborhoodTet where

    toLatex ient = "\\ent"++concatMap braces
                                (show (getTIndex ient)
                                  : map (show . forgetTIndex . ($ ient)) 
                                        [ient_top,ient_bot,ient_left,ient_right]) 
                        
instance Latexable InnerEdgeNeighborhood where toLatex = tuple' . ien_toList 
instance Latexable BoundaryEdgeNeighborhood where toLatex = tuple' . ben_toList 

-- | Ens = edge neighborhoods
latexifyEns :: Triangulation -> Latex
latexifyEns tr = 
    slashes
        [ amps ((either toLatex toLatex $ en)
                    : case (isClosedTriangulation tr, en) of
                           (True,_) -> []
                           (False,Left _) -> [op1 "textit" "(Rand)"]
                           (False,Right _) -> [""])

            | e <- edges tr
            , let en = someEdgeNeighborhood e
            , then sortWith by (either ben_length ien_length en)

                  ]

instance Latexable VectorIndex where
    toLatex i = "v_{"++show (toInteger i)++"}"

align :: Latex -> Latex
align = latexEnv "align*" 
-- aligned = latexEnv "aligned" 

displaymath :: Latex -> Latex
displaymath = latexEnv "displaymath"

newcommand :: Latexable a => [Char] -> Int -> a -> [Char]
newcommand cmd (nargs::Int) expn = "\\newcommand{"++cmd++"}"
    ++ (if nargs==0 then "" else "["++show nargs++"]") ++ "{"++toLatex expn++"}"


unlineses :: [[String]] -> String
unlineses = unlines . concat




(&) ::  (Latexable a, Latexable a1) => a -> a1 -> [Char]
x & y = toLatex x ++"&"++toLatex y

instance (Latexable a, Latexable b) => Latexable (a,b) where
    toLatex (a,b) = tuple [toLatex a, toLatex b]

{-# LANGUAGE TemplateHaskell, OverloadedStrings, StandaloneDeriving, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module DotUtil(
    bendMultiedges, bendMultiedges',
    testDot,
    viewDot,
    productionGraph,
    FLN_Id(..),
    mkNode,
    dot2texBasicFlags,
    mathLabelValue,
    -- * Backslash issues
    backslashPlaceholder,
    printIt',
    -- * Dot2tex attributes
    D2tAttribute(..),
    d2t
    ) where


import Control.Monad
import Data.Bits
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing(printIt,PrintDot)
import Data.GraphViz.Types.Canonical
import Data.Text.Lazy.IO(writeFile)
import Latexable
import Prelude hiding(writeFile)
import System.Exit
import System.SimpleArgs
import Triangulation.CanonOrdered
import TriangulationCxtObject
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy(replace,Text)
import Control.Exception
import HomogenousTuples
import Data.List
import qualified Data.Map as M
import Data.Text.Lazy(pack)
import Data.Maybe
import Control.Applicative
import Data.Time.Clock.POSIX
import THUtil
import PrettyUtil
import Data.GraphViz.Attributes

testDot :: PrintDot a => (Triangulation -> a) -> IO ExitCode
testDot mkdotGraph_ = do
    (nTets,nGluings) <- getArgs
    t <- randomTriangulation nTets nGluings
    putStrLn "\n\n=== TRIANGULATION ===\n"
    print t
    viewDot (mkdotGraph_ t)

dot2texBasicFlags :: [String]
dot2texBasicFlags = [
--    "-ftikz", 
    "--tikzedgelabels", 
    "--autosize", 
    "-traw",
    "--usepdflatex",
    "--nominsize"

--     "--valignmode=dot",
--     "--graphstyle=anchor=base"
    
    ]

viewDot :: PrintDot a => a -> IO ExitCode
viewDot dotGraph0 = do
--     putStrLn "\n\n=== DOTGRAPH ===\n"
--     print dotGraph0 
--     putStrLn "\n\n=== DOT CODE ===\n"
--     putStrLn =<< prettyPrint dotGraph0
     
    fbase <- ("/tmp/viewDot" ++) . show . fromEnum <$> getPOSIXTime

    let 
        dotfile = fbase++".dot"
        texfile = fbase++".tex"
        pdffile = fbase++".pdf"

        
    writeFile dotfile (printIt' dotGraph0)
    rawSystemS "ln" ["-sf",dotfile,"/tmp/it.dot"]

    forM_ [
            -- "pgf",
            "tikz"]
        (\mode -> do
                rawSystemS "dot2tex" (dot2texBasicFlags++["-f",mode,"-o",texfile,dotfile])
                rawSystemS "ln" ["-sf",texfile,"/tmp/it.tex"]
                rawSystemS "pdflatex" ["-interaction","batchmode","-output-directory","/tmp",texfile]
                rawSystemS "ln" ["-sf",pdffile,"/tmp/it.pdf"]
                rawSystemS "okular" [pdffile])

    return ExitSuccess


class FLN_Id a where
    flnId :: a -> Int

tagBits :: Int
tagBits = 3

instance FLN_Id TIndex where
    flnId i = shiftL (fromEnum i) tagBits .|. 3  

faceLatticeNodeId' :: Enum b => Int -> b -> Int
faceLatticeNodeId' tag x = shiftL (fromEnum x) tagBits .|. tag 

instance FLN_Id ITriangle where flnId = faceLatticeNodeId' 2
instance FLN_Id IEdge where flnId = faceLatticeNodeId' 1
instance FLN_Id IVertex where flnId = faceLatticeNodeId' 0 


flnIdO :: (OrderableFace b a, FLN_Id b) => a -> Int
flnIdO = flnId . forgetVertexOrder

instance FLN_Id OITriangle where flnId = flnIdO
instance FLN_Id OIEdge where flnId = flnIdO

deriving instance FLN_Id COIEdge  

flnIdT :: FLN_Id b => T b -> Int
flnIdT = flnId . unT

instance FLN_Id TTriangle where flnId = flnIdT
instance FLN_Id TEdge where flnId = flnIdT
instance FLN_Id TVertex where flnId = flnIdT


mkNode
  :: (Latexable a, FLN_Id a) => [Attribute] -> a -> DotNode Int
mkNode extras x = 
    DotNode (flnId x)
        (
        [
--            Shape theShape,
--            Margin (PVal (createPoint (-0.2) (-0.2))),
            (Label . StrLabel . Text.pack . mathmode . toLatex) x
        ]
        ++ extras)

backslashPlaceholder :: Text
backslashPlaceholder = "BACKSLASH PLACEHOLDER 908213898" 

-- | Replaces 'backslashPlaceholder's in the output with backslashes
printIt' :: PrintDot a => a -> Text
printIt' = s . printIt
    where
        s = replace backslashPlaceholder "\\"

lenLens :: [Attribute] -> (Maybe Double, Double -> [Attribute])
lenLens attrs_ = 
    case break isLen attrs_ of
         (xs, Len l : ys) -> (Just l, \l' -> Len l' : xs ++ ys)
         (_,_:_) -> assert False undefined
         (xs, []) -> (Nothing, \l' -> Len l' : xs)

 where
  isLen (Len _) = True
  isLen _ = False

multiplyLen :: Double -> Double -> [Attribute] -> [Attribute]
multiplyLen _ 1 attrs_ = attrs_ 
multiplyLen default_ factor_ attrs_ = 
    case lenLens attrs_ of
        (v,s) -> s (factor_ * fromMaybe default_ v)
 
bendMultiedges :: (Ord n, Show n, Pretty n) => [DotEdge n] -> [DotEdge n]
bendMultiedges edges_ = 
    let
        endpoints e = $(assrt [|isNondecreasing2 r|] ['r,'edges_]) r 
            where r = (fromNode e, toNode e)

        indexedEdges = zip [0::Int ..] edges_

        edgeCount = 
            foldl'
                (\m (i,e) ->
                    M.insertWith (++) (endpoints e) [i] m) 
                M.empty
                indexedEdges


        --UnknownAttribute "topath" "bend right",
        bendRight :: Int -> Text
        bendRight i = pack ("bend right=" ++ show i) 

        addTheAttrs (i,e) = e { edgeAttributes = extraAttrs ++ 
                                    multiplyLen 1 lenFactor (edgeAttributes e) }

            where 
              (extraAttrs,lenFactor) =
                case edgeCount M.! endpoints e of
                     [_] -> ([],0.82)
                     is -> 
                        let
                            kmax = length is - 1
                            k = fromMaybe (assert False undefined)
                                   (elemIndex i is) 

                            maxAngle | kmax == 1 = 33
                                     | kmax == 2 = 60
                                     | otherwise = 90

                            len_ | kmax == 1 = 1
                                 | kmax == 2 = 1.08
                                 | otherwise = 1.2

                            angle = (-maxAngle) + (2*maxAngle*(kmax-k) `div` kmax)
                        in
                            ([
                                d2t (ToPath (bendRight angle)) 
                             ],

                             len_)
                            



    in
        fmap addTheAttrs indexedEdges 


-- | Modify the 'edgeStmts' with 'bendMultiedges'.
bendMultiedges' :: (Ord n, Show n, Pretty n) => DotGraph n -> DotGraph n
bendMultiedges' dg
    = 
        let
            gs = graphStatements dg
        in
            dg { graphStatements = gs { edgeStmts = bendMultiedges (edgeStmts gs) } } 
        


productionGraph :: PrintDot a => [Char] -> a -> IO ()
productionGraph name graph = do

        let fn ext = "/tmp/" ++ name ++ "." ++ ext

        writeFile (fn "dot") (printIt' graph)

        rawSystemS "dot2tex" (dot2texBasicFlags
            ++[ "-f","tikz",
                "-o",fn "tex",
                "--figonly",
                fn "dot"])

        putStrLn (fn "tex")


instance Pretty Attribute where
    prettyPrec = prettyPrecFromShow

instance Pretty n => Pretty (DotEdge n) where
    prettyPrec prec (DotEdge a b c) = 
        prettyPrecApp prec
            (text "DotEdge")
            [anyPretty a, anyPretty b, anyPretty c]


d2t :: D2tAttribute -> Attribute
d2t (FigPreamble x) = UnknownAttribute "d2tfigpreamble" x
d2t (DocPreamble x) = UnknownAttribute "d2tdocpreamble" x
d2t (LblStyle x) = UnknownAttribute "lblstyle" x
d2t (ToPath x) = UnknownAttribute "topath" x

data D2tAttribute = 
    FigPreamble Text |
    DocPreamble Text |
    LblStyle Text |
    -- ^ Used to set styles for drawing graph, node and edge labels. Only works for the @pgf@ and @tikz@ output formats.
    ToPath Text
    -- ^ Used to set a @to@ path operation for connecting nodes. Only works for the @tikz@ output format.


mathLabelValue :: Latexable a => a -> Label
mathLabelValue = toLabelValue . mathmode

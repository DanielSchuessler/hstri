{-# LANGUAGE TemplateHaskell, OverloadedStrings, StandaloneDeriving, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module DotUtil(
    -- * Reex
    module Data.GraphViz,
    module Data.GraphViz.Attributes.Complete,
    module DotUtil.Lenses,
    PrintDot,

    -- * Misc
    bendMultiedges, bendMultiedges',
    testDot,
    viewDot,
    viewDot',
    productionGraph,
    FLN_Id(..),
    mkNode,
    dot2texBasicFlags,
    mathLabelValue,
    mathLabel,
    Seed(..),
    seed,
    getPosAttrSafe,
    getPosAttr,
    posToVec2,
    -- * Backslash issues
    backslashPlaceholder,
    printIt',
    -- * Dot2tex attributes
    D2tAttribute(..),
    d2t,
    style
    ) where


import Control.Exception
import Control.Monad
import Data.Bits
import Data.GraphViz  hiding(style)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing(printIt,PrintDot)
import Data.List
import Data.Maybe
import Data.Text.Lazy(pack)
import Data.Text.Lazy(replace,Text)
import Data.Text.Lazy.IO(writeFile)
import HomogenousTuples
import Latexable
import Prelude hiding(writeFile)
import PrettyUtil
import System.Exit
import System.SimpleArgs
import THUtil
import Triangulation.CanonOrdered
import Triangulation.Random
import TriangulationCxtObject
import Util
import qualified Data.Map as M
import qualified Data.Text.Lazy as Text
import Data.Vect.Double(Vec2(..))
import DotUtil.Lenses

testDot :: PrintDot a => (Triangulation -> a) -> IO ExitCode
testDot mkdotGraph_ = do
    (nTets,nGluings) <- getArgs
    t <- randomTriangulation nTets nGluings
    putStrLn "\n\n=== TRIANGULATION ===\n"
    print t
    viewDot (mkdotGraph_ t)

dot2texBasicFlags :: [String]
dot2texBasicFlags = [
    "-ftikz", 
    "--tikzedgelabels", 
    "--autosize", 
    "-traw",
    "--usepdflatex",
    "--nominsize"
--    ,"--debug"

--     "--valignmode=dot",
--     "--graphstyle=anchor=base"
    
    ]

viewDot :: PrintDot a => a -> IO ExitCode
viewDot = viewDotCore True 

viewDot' :: PrintDot a => a -> IO ExitCode
viewDot' = viewDotCore False

viewDotCore :: PrintDot a => Bool -> a -> IO ExitCode
viewDotCore useDot2tex dotGraph0 = do
--     putStrLn "\n\n=== DOTGRAPH ===\n"
--     print dotGraph0 
--     putStrLn "\n\n=== DOT CODE ===\n"
--     putStrLn =<< prettyPrint dotGraph0
     
    fbase <- tmpfn "viewDot"

    let 
        dotfile = fbase++".dot"
        texfile = fbase++".tex"
        pdffile = fbase++".pdf"

        
    writeFile dotfile (printIt' dotGraph0)
    rawSystemS "ln" ["-sf",dotfile,"/tmp/it.dot"]

    case useDot2tex of
         True -> do 
            rawSystemS "dot2tex" (dot2texBasicFlags++["-o",texfile,dotfile])
            rawSystemS "source-highlight" ["-i",texfile,"-fesc"]
            rawSystemS "ln" ["-sf",texfile,"/tmp/it.tex"]
            runPdfLatex texfile
         False -> do
             rawSystemS "dot" ["-Tpdf","-o",pdffile,dotfile]

    rawSystemS "ln" ["-sf",pdffile,"/tmp/it.pdf"]
    rawSystemS "pdf-viewer" [pdffile]

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
        (v,s) -> s (factor_ *  fromMaybe default_ (fmap (trace "multiplyLen: modifying Len") v))
 
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
              extraAttrs :: Attributes
              lenFactor :: Double

              (extraAttrs,lenFactor) =
                case $(indxShow) (endpoints e) edgeCount of
                     [_] -> ([],1)--0.82)
                     is -> 
                        let
                            kmax = length is - 1
                            k = fromMaybe (assert False undefined)
                                   (elemIndex i is) 

                            maxAngle | kmax == 1 = 33
                                     | kmax == 2 = 60
                                     | otherwise = 90

                            len_ | kmax == 1 = (1/0.82)
                                 | kmax == 2 = (1.08/0.82)
                                 | otherwise = (1.2/0.82)

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




d2t' :: D2tAttribute -> Attribute
d2t' (FigPreamble x) = UnknownAttribute "d2tfigpreamble" x
d2t' (DocPreamble x) = UnknownAttribute "d2tdocpreamble" x
d2t' (LblStyle x) = UnknownAttribute "lblstyle" x
d2t' (ToPath x) = UnknownAttribute "topath" x

d2t :: D2tAttribute -> Attribute
d2t a = case d2t' a of
             UnknownAttribute b c -> UnknownAttribute b (replace "\\" backslashPlaceholder c)
             _ -> assert False undefined

data D2tAttribute = 
    FigPreamble Text |
    DocPreamble Text |
    LblStyle Text |
    -- ^ Used to set styles for drawing graph, node and edge labels. Only works for the @pgf@ and @tikz@ output formats.
    ToPath Text
    -- ^ Used to set a @to@ path operation for connecting nodes. Only works for the @tikz@ output format.


mathLabelValue :: Latexable a => a -> Label
mathLabelValue = toLabelValue . mathmode

mathLabel :: Latexable a => a -> Attribute
mathLabel = Label . mathLabelValue


newtype Seed = Seed Int
    deriving(Eq,Show,Num)

seed :: Seed -> Attribute
seed (Seed s) = Start (StartStyleSeed RandomStyle s)

style :: Text -> Attribute
style = UnknownAttribute "style" . replace "\\" backslashPlaceholder

getPosAttrSafe :: Attributes -> Maybe Pos
getPosAttrSafe = findJust (\a -> case a of
                                  Pos x -> Just x
                                  _ -> Nothing)

getPosAttr :: Attributes -> Pos
getPosAttr = fromMaybe (assert False undefined) . getPosAttrSafe


posToVec2 :: Pos -> Vec2
posToVec2 (PointPos p) = Vec2 (xCoord p) (yCoord p)
posToVec2 x = $err' ("posToVec2: Can't deal with Point "++show x) 


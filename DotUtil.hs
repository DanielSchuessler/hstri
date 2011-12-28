{-# LANGUAGE OverloadedStrings, StandaloneDeriving, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module DotUtil(testDot,viewDot,FLN_Id(..),mkNode,dot2texBasicFlags,backslashPlaceholder,printIt') where


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
import Triangulation
import TriangulationCxtObject
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy(replace,Text)

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
    
    let fbase   = "/tmp/faceLattice" 
        dotfile = fbase++".dot"
        texfile mode = fbase++"_"++mode++".tex"
        pdffile mode = fbase++"_"++mode++".pdf"

        
    writeFile dotfile (printIt' dotGraph0)

    forM_ [
            -- "pgf",
            "tikz"]
        (\mode -> do
                rawSystemS "dot2tex" (dot2texBasicFlags++["-f",mode,"-o",texfile mode,dotfile])
                systemS ("cd /tmp && pdflatex "++texfile mode)
                systemS ("okular " ++ pdffile mode ++ " &"))

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

{-# LANGUAGE FlexibleContexts, ViewPatterns, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Tikz.Preview(Tikz,previewTikz,previewTikzFile,wrapTikzAsDoc) where

import Data.String.Interpolation
import qualified Data.Map as M
import Latexable
import HomogenousTuples
import Util
import Tikz.Base


previewTikz :: Tikz -> IO ()
previewTikz tikz = do

    fbase <- tmpfn "previewTikz"

    let texfile = fbase++".tex"
        pdffile = fbase++".pdf"

    writeFile texfile (wrapTikzAsDoc tikz)
    rawSystemS "ln" ["-sf",texfile,"/tmp/it.tex"]
    previewTikzFile texfile pdffile

previewTikzFile :: FilePath -> FilePath -> IO ()
previewTikzFile texfile pdffile = do

    runPdfLatex texfile

    runOkularAsync pdffile

wrapTikzAsDoc :: Tikz -> Tikz
wrapTikzAsDoc tikz = [str|
\documentclass[ngerman]{article}

\usepackage{amsmath}
\usepackage{amsthm}
\usepackage{amssymb}
\usepackage[ngerman]{babel}
\usepackage[utf8]{inputenc}
\usepackage{url}
\usepackage{graphicx}
\usepackage{wrapfig}
\usepackage{tikz}
\usepackage[a2paper]{geometry}

\usetikzlibrary{decorations,arrows,shapes,calc}

\begin{document}
$tikz$
\end{document} 
|]

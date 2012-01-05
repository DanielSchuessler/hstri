{-# LANGUAGE FlexibleContexts, ViewPatterns, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Tikz.Preview(Tikz,previewTikz,previewTikzFile) where

import Data.String.Interpolation
import qualified Data.Map as M
import Latexable
import HomogenousTuples
import Util

type Tikz = String

previewTikz :: Tikz -> IO ()
previewTikz tikz = do

    fbase <- tmpfn "previewTikz"

    let texfile = fbase++".tex"
        pdffile = fbase++".pdf"

    writeFile texfile (wrap tikz)
    rawSystemS "ln" ["-sf",texfile,"/tmp/it.tex"]
    previewTikzFile texfile pdffile

previewTikzFile :: FilePath -> FilePath -> IO ()
previewTikzFile texfile pdffile = do

    runPdfLatex texfile

    runOkularAsync pdffile


wrap :: Tikz -> Tikz
wrap tikz = [str|
\documentclass[a4paper,ngerman]{article}

\usepackage{amsmath}
\usepackage{amsthm}
\usepackage{amssymb}
\usepackage[ngerman]{babel}
\usepackage[utf8]{inputenc}
\usepackage{url}
\usepackage{graphicx}
\usepackage{wrapfig}
\usepackage{tikz}

\usetikzlibrary{decorations,arrows,shapes,calc}

\begin{document}
$tikz$
\end{document} 
|]

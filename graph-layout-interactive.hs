{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, ImplicitParams #-}
import Tikz.InteractiveLayout
import ClosedOrCensus6

fileBase = "/tmp/graph-layout-interactive"
texfile = fileBase ++ ".tex"
pdffile = fileBase ++ ".pdf"

main = interactiveLayoutMain tr_0 fileBase 

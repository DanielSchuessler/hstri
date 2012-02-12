import Tikz.Gen
import ClosedOrCensus6
import ReadArgs
import Triangulation.Random
import Test.QuickCheck.Gen



cen = do
    i <- readArgs
    previewAuto (closedOrCensus6 !! i) defaultSGEE


rnd = do
    (n,mingl,maxgl) <- readArgs
    (tr:_) <- sample' (randomManifoldT n mingl maxgl) 
    previewAuto tr defaultSGEE  

main = cen

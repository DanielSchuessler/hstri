import ReadArgs
import Triangulation
import EqvGraphs

main = do
    (nT,nG) <- readArgs 
    tr <- randomTriangulation nT nG
    veg tr



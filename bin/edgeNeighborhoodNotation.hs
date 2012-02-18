import HsTri
import Tikz.Base

main = do
    go 4 False
    go 2 False
    go 1 False
    go 3 True
    putStrLn "done"
        

go n isBoundary =
    writeFile ("/h/dipl/graphs/edgeNeighborhoodNotation"++
            (if isBoundary then "B" else "I")++show n++".tex")
        (tikzpicture []
            (renderEdgeNeighborhood_core 0.7 (mkEn n) isBoundary))



mkEn n = [ map4 (\c -> c:"_"++show i) (fromList4 "abcd") | i <- [0..n-1] ] 

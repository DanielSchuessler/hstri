import HsTri
import ClosedOrCensus6
import qualified Data.Vector as V
import TriArcGraph


go i = do
        --viewDot (vertexEqvGraph tr s)
        putStrLn ("\n"++lbl)
        let ddRes = dd tr
        pr . indent 2 . ppDDRes $ ddRes 
        V.forM_ (ddSolutions' ddRes)
            (\sol -> viewDot (ta tr (Just sol)))


    where
        (lbl,tr) = closedOrCensus6 !! i

import MathUtil
import Test.QuickCheck
import System.Random
import TriArcGraph
import ClosedOrCensus6
import DotUtil
import TriangulationCxtObject
import EqvGraphs
import VerboseDD
import PrettyUtil
import Control.Monad
import qualified Data.Vector as V


go i = do
        --viewDot (vertexEqvGraph tr s)
        putStrLn ("\n"++lbl)
        let ddRes = dd tr
        pr . indent 2 . ppDDRes $ ddRes 
        V.forM_ (ddSolutions' ddRes)
            (\sol -> viewDot (ta tr (Just sol)))


    where
        (lbl,tr) = closedOrCensus6 !! i

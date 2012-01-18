import HsTri
import Orientation
import Util

main = mapM_ f (reverse closedOrCensus6)

f (l,tr) = do
    pr l
    pr (fromRight $ orientTriangulation tr)

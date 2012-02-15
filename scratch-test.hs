import HsTri
import qualified Data.Vector.Generic as VG
import Triangulation.PreTriangulation

-- tr_7
--tr = snd ([ tr | tr <- closedOrCensus6, numberOfTetrahedra tr > 3 ] !! 0)
tr = tr_0

ss = dds tr
ss' = quadToStandardSolSet tr (qVertexSolutions tr) 

res = sscr_final ss'


main = do
--     pr (ppDDRes ss)
--     putStrLn "--"
--     putStrLn (prettyMatrix res)
--     putStrLn . prettyMatrix . VG.map (makeVecIntegral . sd_toVector . adm_coords) $ res
    pr (ppSSCRes ss')



cutq = readRgaFile

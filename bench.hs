import Bench
import Bench.Triangulations
import Criterion.Main

main :: IO ()
main = defaultMain
            [ bench "it" (nf qVertexSolBench trs) ] 


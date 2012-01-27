import Bench.TriangulationGen
import Language.Haskell.TH

main = print =<< runQ trsGen

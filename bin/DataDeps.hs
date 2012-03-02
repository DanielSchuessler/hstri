{-# OPTIONS -Wall #-}
module DataDeps where
import VerboseDD
import Data.Graph.Inductive.Graph
import Util
import Element
import qualified Data.Vector.Generic as VG
import GraphUtil
import Data.Graph.Inductive.Query.DFS
import qualified Data.Set as S
import Data.Graph.Inductive.PatriciaTree(Gr)


data NodeType = Result | RegularNode
    deriving(Eq,Ord)

type StepIx = Int

ddrToGraph
  :: (CoordSys co) =>
     DDResult co
     -> Gr (StepIx, NodeType) ()
ddrToGraph (DDResult _ steps _ _) =
        delNodes (filter (\n -> not (S.member n nodes1)) (nodes gr0)) gr0 

    where
        ix = fi . ddrep_index

        resultNodes = map ix . asList . ddsr_pbs . last $ steps

        resultNodeSet = S.fromList resultNodes 

        lnodes0 = [ (ix v, cluster)
                        | 

                                (ddsr,i) <- zip steps [0..]
                            ,   v <- asList (ddsr_pbs ddsr) 
                            ,   let cluster = 
                                     (i, if ix v `S.member` resultNodeSet
                                            then Result 
                                            else RegularNode) 
                            
                    ]

        ledges0 = do
                    ddsr <- steps
                    PairFate u w (OK v) <- VG.toList (ddsr_pairFates ddsr)
                    [ 
                        (ix u, fi v, ())
                      , (ix w, fi v, ())
                     ]

        gr0 = idPatGr $ mkGraph lnodes0 ledges0 


        nodes1 = S.fromList (rdfs resultNodes gr0)


{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}
module GraphUtil where

import Data.Graph.Inductive hiding(augmentGraph)
import Control.Arrow
import Data.GraphViz
import Data.GraphViz.Printing(printIt)
import Data.GraphViz.Commands.IO
import Control.Monad
import Data.Text.Lazy.IO

mapCxt :: (Node -> Node) -> (a -> a') -> (b -> b') -> Context a b -> Context a' b'
mapCxt fn fa fb (adj, n, a, adj') = 
    (fmap g adj,
     fn n,
     fa a,
     fmap g adj')
 where
    g = fb *** fn

disjointUnionGraphs :: (a -> a'') -> (b -> b'') -> (a' -> a'') -> (b' -> b'') -> Gr a b -> Gr a' b' -> Gr a'' b''
disjointUnionGraphs fa fb fa' fb' g g' =
    let
        (_,n) = nodeRange g

        c cxt r = mapCxt (+n) fa' fb' cxt & r
    in
        ufold c (gmap (mapCxt id fa fb) g) g'

unlabel :: LNode b -> Node
unlabel = fst

nodeLabel :: LNode b -> b
nodeLabel = snd




-- copied from the graphviz package
-- | Pass the 'DotRepr' through the *chosen* command and then augment
--   the 'Graph' that it came from.
dotAttributesWithCmd :: (Graph gr, PPDotRepr dg Node) => 
    GraphvizCommand 
    -> gr nl (EdgeID el)
    -> dg Node 
    -> IO (gr (AttributeNode nl) (AttributeEdge el))
dotAttributesWithCmd command gr dot
  =  do
        dg <- graphvizWithHandle command dot DotOutput hGetDot
        Data.Text.Lazy.IO.putStrLn $ printIt dg
        return ((augmentGraph gr . parseDG) dg)
  where
    parseDG = asTypeOf dot


-- copied from the graphviz package
-- | Run the *chosen* Graphviz command on the graph to get
--   positional information and then combine that information back
--   into the original graph.
graphToGraphWithCmd :: (Ord cl, Graph gr) => 
    GraphvizCommand 
    -> GraphvizParams Node nl el cl l 
    -> gr nl el
    -> IO (gr (AttributeNode nl) (AttributeEdge el))
graphToGraphWithCmd command params gr = dotAttributesWithCmd command gr' dot
  where
    dot = graphToDot params' gr'
    params' = params { fmtEdge = setEdgeIDAttribute $ fmtEdge params }
    gr' = addEdgeIDs gr

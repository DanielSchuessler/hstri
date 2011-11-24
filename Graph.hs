{-# LANGUAGE TypeFamilies, FlexibleContexts, NoMonomorphismRestriction #-}
-- | Wrapper around "Data.Graph"
module Graph where

import qualified Data.Graph as G
import Data.Graph(Vertex,graphFromEdges)
import Data.Functor

class Ord (GraphKey a) => HasGraphKey a where
    type GraphKey a
    toGraphKey :: a -> GraphKey a


newtype Graph node = Graph ( G.Graph 
    , Vertex -> (node, GraphKey node, [GraphKey node]) 
    , GraphKey node -> Maybe Vertex )

transformAdjacencyList :: (HasGraphKey node) => [(node, [node])] -> [(node,GraphKey node, [GraphKey node])]
transformAdjacencyList xs = [(v, toGraphKey v, toGraphKey <$> us) | (v,us) <- xs]

mkGraph ::  HasGraphKey node => [(node, [node])] -> Graph node
mkGraph = Graph . graphFromEdges . transformAdjacencyList 



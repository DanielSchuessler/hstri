{-# LANGUAGE TupleSections #-}
module Data.EdgeLabelledTree where

import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict
import Data.Maybe
import PrettyUtil
import Util
import qualified Data.Set as S


data EdgeLabelledTree n e = Node n [(e,EdgeLabelledTree n e)]
    deriving Show

dfs :: Ord n => n -> (n -> [(e,n)]) -> EdgeLabelledTree n e
dfs n0 outEdges = evalState (goUnseen n0) S.empty
    where
        go n = do
            isUnseen_ <- isUnseen n
            if isUnseen_
               then Just <$> goUnseen n 
               else return Nothing

        goUnseen n =
                do
                    modify (S.insert n)
                    Node n <$> mapMaybeM goEdge (outEdges n)

        isUnseen n = gets (not . S.member n)
        
        goEdge (e,n) = fmap (e,) <$> go n

instance (Pretty n, Pretty e) => Pretty (EdgeLabelledTree n e) where

    prettyPrec prec (Node n es) = 
        prettyPrecApp prec "Node" [anyPretty n, anyPretty es] 


eltFold :: (n -> [(e, r)] -> r) -> EdgeLabelledTree n e -> r
eltFold k (Node n es) = k n (map (second (eltFold k)) es)

eltFoldM :: Monad m => (n -> [(e, r)] -> m r) -> EdgeLabelledTree n e -> m r
eltFoldM k = go 
    where
        go (Node n es) = k n =<< mapM (\(e,elt) -> (e,) `liftM` go elt) es

eltNodes :: EdgeLabelledTree n e -> [n]
eltNodes = eltFold (\n ers -> n : concatMap snd ers)  

eltEdges :: EdgeLabelledTree n e -> [e]
eltEdges = eltFold . const . concatMap . uncurry $ (:)

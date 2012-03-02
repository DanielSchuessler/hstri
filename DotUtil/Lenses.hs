{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module DotUtil.Lenses where
import Data.Lens.Template
import Data.GraphViz.Types.Canonical
import Language.Haskell.TH.Syntax
import Text.Groom
import Language.Haskell.TH
import Data.Lens.Common
import Data.GraphViz.Attributes

-- TH Bug, using -ddump-splices output
-- nameMakeLens ''DotGraph (Just . (++"L"))

strictGraphL :: forall n. Lens (DotGraph n) Bool
strictGraphL = lens strictGraph (\ x s -> s {strictGraph = x})
directedGraphL :: forall n. Lens (DotGraph n) Bool
directedGraphL
    = lens directedGraph (\ x s -> s {directedGraph = x})
graphIDL :: forall n. Lens (DotGraph n) (Maybe GraphID)
graphIDL = lens graphID (\ x s -> s {graphID = x})
graphStatementsL :: forall n. Lens (DotGraph n) (DotStatements n)
graphStatementsL
    = lens graphStatements (\ x s -> s {graphStatements = x})

-- nameMakeLens ''DotStatements (Just . (++"L"))

attrStmtsL :: forall n. Lens (DotStatements n) [GlobalAttributes]
attrStmtsL = lens attrStmts (\ x s -> s {attrStmts = x})
subGraphsL :: forall n. Lens (DotStatements n) [DotSubGraph n]
subGraphsL = lens subGraphs (\ x s -> s {subGraphs = x})
nodeStmtsL :: forall n. Lens (DotStatements n) [DotNode n]
nodeStmtsL = lens nodeStmts (\ x s -> s {nodeStmts = x})
edgeStmtsL :: forall n. Lens (DotStatements n) [DotEdge n]
edgeStmtsL = lens edgeStmts (\ x s -> s {edgeStmts = x})

-- nameMakeLens ''DotSubGraph (Just . (++"L"))

isClusterL :: forall n. Lens (DotSubGraph n) Bool
isClusterL = lens isCluster (\ x s -> s {isCluster = x})
subGraphIDL :: forall n. Lens (DotSubGraph n) (Maybe GraphID)
subGraphIDL = lens subGraphID (\ x s -> s {subGraphID = x})
subGraphStmtsL :: forall n. Lens (DotSubGraph n) (DotStatements n)
subGraphStmtsL
    = lens subGraphStmts (\ x s -> s {subGraphStmts = x})

-- nameMakeLens ''GlobalAttributes (Just . (++"L"))

attrsL :: Lens GlobalAttributes Attributes
attrsL = lens attrs (\ x s -> s {attrs = x})


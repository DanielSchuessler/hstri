{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, FunctionalDependencies, MultiParamTypeClasses, ImplicitParams, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ExistentialQuantification #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module FaceLattice2 where


import AbstractTetrahedron
import Control.DeepSeq
import Control.Exception
import Data.Colour as Colour
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB as SRGB
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz as GraphViz
import Data.GraphViz.Attributes.Colors as GraphVizColors
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing(printIt)
import Data.List(genericLength)
import Data.Maybe
import IndexedSimplices
import Latexable
import QuickCheckUtil
import System.Exit
import System.Process
import System.SimpleArgs(getArgs)
import TriangulationCxtObject
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as TextIO
import Control.Applicative
import DotUtil


theShape :: Shape
theShape = BoxShape

dotGraph
  :: Triangulation -> DotGraph Node
dotGraph tr = 
    DotGraph {
        strictGraph = False,
        directedGraph = True,
        graphID = Nothing,
        graphStatements = DotStmts {

            attrStmts = 
                [   EdgeAttrs (
                         [PenWidth 2] 
                    )
                ,   GraphAttrs ( 
                        [ 
                                 RankDir FromBottom
--                                , RankSep [2.4] 
                                , Overlap RemoveOverlaps
                               , Splines SplineEdges
                               , Layout "fdp"
                         ] 
                         )
                ,   NodeAttrs [ PenWidth 2 ]
                ],

            subGraphs = [],

            nodeStmts = 
                (mkNode <$> tets_) ++
                (mkNode <$> tris_) ++
                (mkNode <$> edges_) ++
                (mkNode <$> verts_),

            edgeStmts = subfaceEdges ++ equivalenceEdges
        }
    }
  where
    tets_ = tTetrahedra_ tr
    tris_ = canonicallyOrderedTriangles tr
    edges_ = canonicallyOrderedEdges tr
    verts_ = tIVertices tr

    subfaceEdges = 
        [ subfaceEdge y x | x <- tets_  , y <- triangleList x   ] 
     ++ [ subfaceEdge y x | x <- tris_  , y <- edgeList x       ] 
     ++ [ subfaceEdge y x | x <- edges_ , y <- vertexList x     ] 


    equivalenceEdges = 
        tOriginalGluings tr >>=
            (\gl -> 
                equivalenceEdge (fst gl) (snd gl) 
                :
                fmap (uncurry equivalenceEdge) (inducedEdgeEquivalences gl)
                ++
                fmap (uncurry equivalenceEdge) (inducedVertexEquivalences gl)
                )




                

subfaceEdge :: (FLN_Id x, FLN_Id y) => x -> y -> DotEdge Node
subfaceEdge x y = 
    DotEdge 
        (flnId x)
        (flnId y)
        [
            Len 0.2
        ]
        
equivalenceEdge :: (FLN_Id x, FLN_Id y) => x -> y -> DotEdge Node
equivalenceEdge x y = 
    DotEdge 
        (flnId x)
        (flnId y)
        [
            Dir NoDir
        ,   Constraint False
        ,   Len 0.01
        ,   Color [X11Color Orange] 
        ,   Weight 10
        ]



white :: Color
white = X11Color White

viewFL :: Triangulation -> IO ExitCode
viewFL t = viewDot (dotGraph t)

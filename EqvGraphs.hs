{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, FunctionalDependencies, MultiParamTypeClasses, ImplicitParams, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ExistentialQuantification #-}
{-# OPTIONS -Wall #-}
module EqvGraphs
    (edgeEqvGraph,
    vertexEqvGraph,veg,
    viewEdgeEqvGraph,
    viewVertexEqvGraph,
    productionEqvGraphs) 

    where


import Control.Applicative
import Data.GraphViz as GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Maybe
import DotUtil
import HomogenousTuples
import Latexable
import Prelude hiding(writeFile)
import System.Exit
import Triangulation.CanonOrdered


graphHelper
  :: (Latexable a, FLN_Id a, FLN_Id b, Ord b) =>
     (Triangulation -> [a])
     -> (Gluing -> [(b, b)]) -> Triangulation -> DotGraph Int
graphHelper things inducedEquivalences tr = 
    DotGraph {
        strictGraph = False,
        directedGraph = False,
        graphID = Nothing,
        graphStatements = DotStmts {

            attrStmts = 
                [   EdgeAttrs gEdgeAttrs
                ,   GraphAttrs gGraphAttrs 
                ,   NodeAttrs gNodeAttrs
                ],

            subGraphs = [],

            nodeStmts = 
                (mkNode [] <$> things tr),

            edgeStmts = 
                bendMultiedges 
                    (

                tOriginalGluings tr >>=
                    (\gl -> 
                        fmap (equivalenceEdge gl) (inducedEquivalences gl)
                        )


                    )
        }
    }



edgeEqvGraph :: Triangulation -> DotGraph Int
edgeEqvGraph = graphHelper tCOIEdges inducedEdgeEquivalences 

vertexEqvGraph :: Triangulation -> DotGraph Int
vertexEqvGraph = graphHelper tIVertices inducedVertexEquivalences


mkLabel :: [Char] -> Label
mkLabel = toLabelValue . mathmode

gEdgeAttrs :: [Attribute]
gEdgeAttrs = 
                         [
                            d2t (LblStyle "sloped"),
                            FontSize 9
                         ] 

gNodeAttrs :: [Attribute]
gNodeAttrs = 
    [
        let hor = 0.02 in Margin (PVal (createPoint hor (hor*2/3))) 
    ,   PenWidth 2
    ]


gGraphAttrs :: [Attribute]
gGraphAttrs =
                        [ 
                                 RankDir FromLeft
--                                , RankSep [2.4] 
                               , Overlap RemoveOverlaps
--                               , Splines SplineEdges
                               , Layout "neato"
                               , Pack (PackMargin 4)
                               , Start (StartStyleSeed RandomStyle 1)
                               , d2t (FigPreamble $
                                    (backslashPlaceholder `mappend` "footnotesize"))
                         ] 

equivalenceEdge
  :: (Ord a, FLN_Id a) => Gluing -> (a, a) -> DotEdge Int
equivalenceEdge gl = equivalenceEdge' gl . sort2

equivalenceEdge'
  :: (FLN_Id a) => Gluing -> (a, a) -> DotEdge Int
equivalenceEdge' ((tri,otri) :: Gluing) (x, y) = 
    DotEdge 
        (flnId x)
        (flnId y)
        [
            (Label . mkLabel)
                (latexTwoRows (toLatex tri ++ "\\sim") otri)
        ]



viewEdgeEqvGraph :: Triangulation -> IO ExitCode
viewEdgeEqvGraph = viewDot . edgeEqvGraph

viewVertexEqvGraph :: Triangulation -> IO ExitCode
viewVertexEqvGraph = viewDot . vertexEqvGraph

veg :: Triangulation -> IO ExitCode
veg = viewVertexEqvGraph

productionEqvGraphs :: [Char] -> Triangulation -> IO ()
productionEqvGraphs trName tr = do
    go "EdgeGraph" edgeEqvGraph
    go "VertexGraph" vertexEqvGraph
  where
    go n f = productionGraph (trName ++ n) (f tr)


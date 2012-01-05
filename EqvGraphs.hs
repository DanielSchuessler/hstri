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
  :: (Ord a1, Latexable a, FLN_Id a, FLN_Id a1) =>
     (Triangulation -> [a])
     -> (Gluing -> [(a1, a1)]) -> Triangulation -> Seed -> DotGraph Int
graphHelper things inducedEquivalences tr theseed = 
    DotGraph {
        strictGraph = False,
        directedGraph = False,
        graphID = Nothing,
        graphStatements = DotStmts {

            attrStmts = 
                [   EdgeAttrs gEdgeAttrs
                ,   GraphAttrs (gGraphAttrs++[seed theseed]) 
                ,   NodeAttrs gNodeAttrs
                ],

            subGraphs = [],

            nodeStmts = 
                (mkNode [] <$> things tr),

            edgeStmts = 
                    (

                tGluingsIrredundant tr >>=
                    (\gl -> 
                        fmap (equivalenceEdge gl) (inducedEquivalences gl)
                        )


                    )
        }
    }



edgeEqvGraph :: Triangulation -> Seed -> DotGraph Int
edgeEqvGraph = graphHelper tCOIEdges inducedEdgeEquivalences 

vertexEqvGraph :: Triangulation -> Seed -> DotGraph Int
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
    ,   Shape Triangle
    ]


gGraphAttrs :: [Attribute]
gGraphAttrs =
                        [ 
--                                 RankDir FromLeft
--                                , RankSep [2.4] 
--                               , Overlap RemoveOverlaps
--                               , Splines SplineEdges
                                 Layout "neato"
--                               , Pack (PackMargin 4)
                               , d2t (FigPreamble $
                                    ("\\footnotesize"))
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
        , Len 1.6
        ]



viewEdgeEqvGraph :: Triangulation -> Seed -> IO ExitCode
viewEdgeEqvGraph = fmap viewDot . edgeEqvGraph

viewVertexEqvGraph :: Triangulation -> Seed -> IO ExitCode
viewVertexEqvGraph = fmap viewDot . vertexEqvGraph

veg :: Triangulation -> Seed -> IO ExitCode
veg = viewVertexEqvGraph

productionEqvGraphs
  :: Seed -> Seed -> [Char] -> Triangulation -> IO ()
productionEqvGraphs seedE seedV trName tr = do
    go "EdgeGraph" edgeEqvGraph seedE
    go "VertexGraph" vertexEqvGraph seedV
  where
    go n f s = productionGraph (trName ++ n) (f tr s)


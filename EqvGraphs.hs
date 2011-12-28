{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, FunctionalDependencies, MultiParamTypeClasses, ImplicitParams, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ExistentialQuantification #-}
{-# OPTIONS -Wall #-}
module EqvGraphs(edgeEqvGraph,vertexEqvGraph,viewEdgeEqvGraph,viewVertexEqvGraph,productionEqvGraphs) where


import Data.GraphViz as GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Maybe
import Latexable
import System.Exit
import TriangulationCxtObject
import Control.Applicative
import DotUtil
import qualified Data.Map as M
import HomogenousTuples
import Data.List(foldl',elemIndex)
import Control.Exception
import Data.Text.Lazy(Text,pack)
import Data.Text.Lazy.IO(writeFile)
import Prelude hiding(writeFile)

fixEdges :: (Ord n, Show n) => [DotEdge n] -> [DotEdge n]
fixEdges edges_ = 
    let
        endpoints e = assert (isOrdered2 r) r where r = (fromNode e, toNode e)

        indexedEdges = zip [0::Int ..] edges_

        edgeCount = 
            foldl'
                (\m (i,e) ->
                    M.insertWith (++) (endpoints e) [i] m) 
                M.empty
                indexedEdges


        --UnknownAttribute "topath" "bend right",
        bendRight :: Int -> Text
        bendRight i = pack ("bend right=" ++ show i) 

        addTheAttrs (i,e) = e { edgeAttributes = extraAttrs ++ edgeAttributes e }

            where 
              extraAttrs =
                case edgeCount M.! endpoints e of
                     [_] -> [ Len 0.82 ]
                     is -> 
                        let
                            kmax = length is - 1
                            k = fromMaybe (assert False undefined)
                                   (elemIndex i is) 

                            maxAngle | kmax == 1 = 33
                                     | kmax == 2 = 60
                                     | otherwise = 90

                            len_ | kmax == 1 = 1
                                 | kmax == 2 = 1.08
                                 | otherwise = 1.2

                            angle = (-maxAngle) + (2*maxAngle*(kmax-k) `div` kmax)
                        in
                            [
                                Len len_,
                                UnknownAttribute "topath" (bendRight angle) 
                            ]
                            



    in
        fmap addTheAttrs indexedEdges 

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
                [   EdgeAttrs (
                         [
--                            PenWidth 2
                         ] 
                    )
                ,   GraphAttrs ( 
                        [ 
                                 RankDir FromLeft
--                                , RankSep [2.4] 
                               , Overlap RemoveOverlaps
--                               , Splines SplineEdges
                               , Layout "neato"
                               , Pack (PackMargin 4)
                               , Start (StartStyleSeed RandomStyle 1)
                               , UnknownAttribute "d2tfigpreamble" 
                                    (backslashPlaceholder `mappend` "footnotesize")
                         ] 
                         )
                ,   NodeAttrs [ PenWidth 2 ]
                ],

            subGraphs = [],

            nodeStmts = 
                (mkNode extraNodeAttrs <$> things tr),

            edgeStmts = 
                fixEdges 
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

extraNodeAttrs :: [Attribute]
extraNodeAttrs = 
    [
        let hor = 0.02 in Margin (PVal (createPoint hor (hor*2/3))) 
    ]
--     [
--         Width (h*a), 
--         Height h, 
--         FixedSize True
--     ]
--  where
--     h=0.3
--     a=5/4

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
--             Decorate False,
--             LabelFloat False,
            UnknownAttribute "lblstyle" "sloped",
            FontSize 9,
--             HeadLabel (mkLabel (toLatex tri)),
--             Label (mkLabel "\\sim"),
--             TailLabel (mkLabel (toLatex otri))
            (Label . mkLabel)
                (
                    --toLatex tri ++ " \\sim " ++ toLatex otri
                    "\\begin{matrix}"++toLatex tri ++ "\\sim\\\\"++toLatex otri++"\\end{matrix}"
                    
                )
        ]



viewEdgeEqvGraph :: Triangulation -> IO ExitCode
viewEdgeEqvGraph = viewDot . edgeEqvGraph

viewVertexEqvGraph :: Triangulation -> IO ExitCode
viewVertexEqvGraph = viewDot . vertexEqvGraph

productionEqvGraphs :: [Char] -> Triangulation -> IO ()
productionEqvGraphs trName tr = do
    go "EdgeGraph" edgeEqvGraph
    go "VertexGraph" vertexEqvGraph

  where

    go name mkgraph = do

        let fn ext = "/tmp/" ++ trName ++ name ++ "." ++ ext

        writeFile (fn "dot") (printIt' . mkgraph $ tr)

        rawSystemS "dot2tex" (dot2texBasicFlags
            ++[ "-f","tikz",
                "-o",fn "tex",
                "--figonly",
                fn "dot"])

        putStrLn (fn "tex")



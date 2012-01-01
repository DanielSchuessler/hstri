{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, FunctionalDependencies, MultiParamTypeClasses, ImplicitParams, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ExistentialQuantification #-}
module TriArcGraph where


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
import TriangulationCxtObject
import Data.Graph.Inductive
import QuadCoordinates
import Control.Arrow((&&&))

ta_toDot gr me = bendMultiedges' $ graphToDot params gr
    where
        params = nonClusteredParams {

            isDirected = True,

            globalAttributes = [
                GraphAttrs gattrs, 
                EdgeAttrs eattrs,
                NodeAttrs nattrs 
              ],

            fmtNode = (\(_,int) -> [ toLabel (mathmode (iNormalTriGetVertex int)) ]),

            fmtEdge = fmtEdge_ 
       
        }

        gattrs = [ Layout "neato" 
                 , FontSize 10 
                 ]

        eattrs = [ 
                 ]

        nattrs = [ Shape Triangle
                 , Margin (DVal 0)
                 ]

        fmtEdge_ (_,_,na@(InnNA ina1 ina2)) =
                [ 
                  Len 1.3
--                , Label (mathLabelValue (toLatex (coeffArc ina1) ++ " \\to " ++ toLatex (coeffArc ina2)))
                , Label (mathLabelValue 
                            (prCoeff (coeffArc ina1) "x_{q_t}"
                             ++
                             prCoeff (coeffArc ina1) "x_{q_h}"))
                , d2t (LblStyle "sloped,above,allow upside down")
                ]
                
        coeff = quad_coefficient me

        coeffArc = coeff . iNormalQuadByNormalArc
                

        addMeToLabel ina1 ina2 lbl = f ina1 ++ "," ++ f ina2
            where
                f ina = 
                    case coeffArc ina of
                        r | r == 0 -> ""
                          | otherwise -> colorBySign r


prCoeff c = case () of 
                                 _ | c == 0 -> const ""
                                   | c == -1 -> ("-" ++)
                                   | c == 1 -> ("+" ++)
                                   | c < 0 -> (("-" ++ toLatex (-c)) ++)
                                   | otherwise -> (("+" ++ toLatex c) ++)

colorBySign r | r < 0 = textcolor ("red"::String) r 
              | otherwise = toLatex r

ta_tr tr me = ta_toDot (triArcGraph tr) me

test_tr tr i = viewDot (ta_tr tr (qMatchingEquationsInteger tr !! i))

test = test_tr tr_l31


normalStuffGraph tr =
    let
        ns = (Left <$> tINormalDiscs tr)
             ++
             (Right <$> innNAs tr)
    in
        mkGraph (map (fromEnum&&&id) ns) 




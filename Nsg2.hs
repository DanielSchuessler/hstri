{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, FunctionalDependencies, MultiParamTypeClasses, ImplicitParams, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ExistentialQuantification #-}
module Nsg2 where


import Control.Applicative
import Control.Arrow((&&&))
import Data.Graph.Inductive
import Data.GraphViz as GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Maybe
import DotUtil
import GraphUtil
import HomogenousTuples
import Latexable
import Data.Numbering
import Prelude hiding(writeFile)
import QuadCoordinates
import System.Exit
import Triangulation.CanonOrdered
import TriangulationCxtObject




normalStuffGraph tr =
    let
        ns = tINormalDiscs tr

        nu = tINormalDiscNu tr

        es = do
            a <- innNAs tr
            d1 <- asList $ iNormalDiscsContainingNormalArc (innNA_fst a) 
            d2 <- asList $ iNormalDiscsContainingNormalArc (innNA_snd a) 

            return (d1,d2,(d1,d2,a))
    in
        mkGraphWithNu nu ns es


nsg_toDot gr me = 
--        bendMultiedges' $ 
        graphToDot params gr
    where
        params = nonClusteredParams {

            isDirected = False,

            globalAttributes = [
                GraphAttrs gattrs, 
                EdgeAttrs eattrs,
                NodeAttrs nattrs 
              ],

            fmtNode = fmtNode_,

            fmtEdge = fmtEdge_ 
       
        }

        gattrs = [ Layout "fdp" 
                 , FontSize 10 
                 , Overlap RemoveOverlaps
--                 , Splines SplineEdges
                 , d2t (FigPreamble (backslashPlaceholder `mappend` "small"))
                 , d2t (DocPreamble (backslashPlaceholder `mappend` "usepackage[a2paper]{geometry}"))
                 , seed 3
                 , Mode KK
                 ]

        eattrs = [ 
--                    Concentrate True
                 ]

        nattrs = [ 
                     Margin (DVal 0)
                 ]

        fmtNode_ (_,nd) = 
                     let
                        shape = eitherIND (const Triangle) (const DiamondShape) nd
                     in
                        [   Shape shape
                        ,   mathLabel nd
                        ]
                        ++ if isTri nd
                              then [ UnknownAttribute "style" "red" ]
                              else []
                        
                        
                        
                    


        fmtEdge_ (_,_,(d1,d2,a)) =
            case map2 isTri (d1, d2) of
                 (True,True) -> 
                    [ 
                      Len 1,
                      UnknownAttribute "style" "thick,red"
                    , Weight 5
                    ]
                 (b1,b2) | b1 /= b2 ->
                    [ UnknownAttribute "style" "blue"
                     , Len 3
                    , Weight 0.1
                    ]

                 _ -> [ 
                         Len 2,
                       Weight 1
                      ]
                
        coeff = quad_coefficient me

                
nsg_tr tr me = nsg_toDot (idGr $ normalStuffGraph tr) me


test_tr tr i = viewDot (nsg_tr tr (qMatchingEquationsInteger tr !! i))
test = test_tr tr_l31


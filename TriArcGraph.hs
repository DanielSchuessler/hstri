{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, FunctionalDependencies, MultiParamTypeClasses, ImplicitParams, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ExistentialQuantification #-}
module TriArcGraph(test,ta_toDot,triArcGraphDot,triArcGraphDotNoQuad,taq) where


import Data.AdditiveGroup
import Data.Char
import Data.Graph.Inductive
import Data.Vector(Vector)
import DotUtil
import Latexable
import MathUtil
import Prelude hiding(writeFile)
import QuadCoordinates
import Triangulation.CanonOrdered
import TriangulationCxtObject
import ZeroDefaultMap
import qualified Data.Vector as V
import qualified Data.Text.Lazy as Text
import PrettyUtil
import Data.Maybe
import Control.Monad



quadVar i = Variable [chr (ord 'a' + i)]

ta_toDot gr mbQuadVector = graphToDot params gr
    where

        params = nonClusteredParams {

            isDirected = True, -- isJust mbQuadVector,

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
                 , d2t (DocPreamble (
                            Text.unwords [
--                              "\\input{/usr/local/share/tikztri.tex}"
                              "\\usetikzlibrary{decorations.markings}" 
                            , "\\usepackage[a2paper]{geometry}"

                             ] 
                             ))
                            
                 ]

        eattrs = [ 
--                      
--                     , 
                   
--                    , style "decoration={expanding waves,angle=4,segment length=2pt},decorate" 
                 ]
                 ++(
                     case mbQuadVector of
                          Nothing -> [Dir NoDir, 
                            d2t (LblStyle "sloped")]
                          Just _ -> []

                    )

        nattrs = [ Shape Triangle
                 , Margin (DVal 0)
                 ]

        fmtEdge_ (_,_,(InnNA ina1 ina2)) =




                   case mbQuadVector of
                        Nothing -> [ 
                        
                            mathLabel 
                             (latexTwoRows (toLatex (iNormalArcGetTriangle ina1) ++ "\\sim") 
                                (iNormalArcGetTriangle ina2))
                            
                            , Len 1.6 ]
                        Just v ->
                            let 
                                coeff q = v `quad_coefficient` q
                                coeffArc = coeff . iNormalQuadByNormalArc
                                c = coeffArc ina2 ^-^ coeffArc ina1
                            in
                                Len 1.3 
                                :
                                if c==zeroV
                                   then
                                    [
                                          Dir NoDir
                                    ]
                                   else
                                    [ 
                                      style markings
                                    , d2t (LblStyle "fill=gray!30,circle")
                                    , mathLabel c 
                                    ]

                

markings = "decoration={markings,mark=between positions 0 and 1 step 0.1 with {\\arrow{stealth}}},postaction=decorate"
                


triArcGraphDot
  :: (AdditiveGroup a, Eq a, Latexable a, Num a) =>
     Triangulation -> Maybe (QuadCoordinates a) -> DotGraph Node
triArcGraphDot tr mbQuadVector = ta_toDot (triArcGraph tr) mbQuadVector

triArcGraphDotNoQuad :: Triangulation -> DotGraph Node
triArcGraphDotNoQuad tr = triArcGraphDot tr (Nothing :: Maybe (QuadCoordinates Integer))


ta_symSol tr me = triArcGraphDot tr (Just . quad_fromVector tr . snd $ symbolicSolution me quadVar)

taq tr quad = viewDot (triArcGraphDot tr (Just quad)) 

test tr = 
    let
        mes = (qMatchingEquationsMatrixRat tr)
    in do
        case symbolicSolution mes quadVar of
             ((mtx,_,_),s) -> do
                viewDot $ ta_toDot (triArcGraph tr) (Just (quad_fromVector tr s))
                print tr
                putStrLn (prettyMatrix mes)
                putStrLn "~>"
                putStrLn (prettyMatrix mtx)
                putStrLn "s="
                print s

odd_tr = mkTriangulation 1 [(0 ./ tABD, 0 ./ oCAD), (0 ./ tABC, 0 ./ oCBD)]

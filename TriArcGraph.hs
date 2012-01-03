{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, FunctionalDependencies, MultiParamTypeClasses, ImplicitParams, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ExistentialQuantification #-}
module TriArcGraph(test) where


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



quadVar i = Variable [chr (ord 'a' + i)]

-- symbolicSolution :: (Ord r, Fractional r) => 
--     QuadCoordinateFunctional r -> QuadCoordinates (ZeroDefaultMap Variable r)
-- symbolicSolution me =
--     case quad_toNonzeroAssocs me of
--          [] -> error "matching equation is zero" 
--          ((q0,r0):qrs) -> 
-- 
--             quad_fromAssocs (
--                 (q0, zdm_fromAssocs [ (quadVar q, -r/r0) | (q,r) <- qrs ])
-- 
--                     :
--                 [ (q, zdm_singleton (quadVar q) 1) | (q,_) <- qrs ]
--                 )


ta_toDot
  :: (Num a, Ord a, AdditiveGroup a, Graph gr, Latexable a) =>
     gr INormalTri InnNA
     -> Vector (ZeroDefaultMap Variable a) -> DotGraph Node
ta_toDot gr symSol = graphToDot params gr
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
                 , d2t (DocPreamble (
                            Text.unwords [
                              "\\input{/usr/local/share/tikztri.tex}"
                             ,"\\usetikzlibrary{decorations.markings}" 
                             ] 
                             ))
                            
                 ]

        eattrs = [ 
--                      Dir NoDir
--                     , 
                    d2t (LblStyle "red,above,sloped")
                  , style "decoration={markings,mark=between positions 0 and 1 step 0.1 with {\\arrow{stealth}}},postaction=decorate"
--                    , style "decoration={expanding waves,angle=4,segment length=2pt},decorate" 
                 ]

        nattrs = [ Shape Triangle
                 , Margin (DVal 0)
                 ]

        fmtEdge_ (_,_,(InnNA ina1 ina2)) =



                [ 
                  Len 1.3
--                , Label (mathLabelValue (toLatex (coeffArc ina1) ++ " \\to " ++ toLatex (coeffArc ina2)))
                , Label (mathLabelValue 
                            (coeffArc ina2 ^-^ coeffArc ina1))

                ]
                
        coeff q = symSol V.! fromEnum q

        coeffArc = coeff . iNormalQuadByNormalArc
                




ta_tr tr me = ta_toDot (triArcGraph tr) (snd $ symbolicSolution me quadVar)


test i tr = 
    let
        mes = (qMatchingEquationsMatrixRat tr)
    in do
        case symbolicSolution mes quadVar of
             ((mtx,_,_),s) -> do
                viewDot $ ta_toDot (triArcGraph tr) s
                print tr
                putStrLn (prettyMatrix mes)
                putStrLn "~>"
                putStrLn (prettyMatrix mtx)
                putStrLn "s="
                print s

odd_tr = mkTriangulation 1 [(0 ./ tABD, 0 ./ oCAD), (0 ./ tABC, 0 ./ oCBD)]

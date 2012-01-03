{-# LANGUAGE OverloadedStrings, FlexibleInstances, TupleSections, FunctionalDependencies, MultiParamTypeClasses, ImplicitParams, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, TypeSynonymInstances, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving, ExistentialQuantification #-}
module Nsg3 where


import Control.Applicative
import Control.Arrow((&&&))
import Data.Graph.Inductive
import Data.Maybe
import DotUtil
import GraphUtil
import HomogenousTuples
import Latexable
import Numbering2
import Prelude hiding(writeFile)
import QuadCoordinates
import System.Exit
import Triangulation.CanonOrdered
import TriangulationCxtObject
import Control.Monad




normalStuffGraph tr me =
    let
        hasNonzeroCoeffOrIsTri = eitherIND 
                                    (const True) 
                                    (\q -> coeff q /= 0)       

        ns = 
                (Left <$> innNAs tr)
             ++
                (Right <$> filter hasNonzeroCoeffOrIsTri
                                  (tINormalDiscs tr))

        coeff = quad_coefficient me

        nu = eitherNu (innNANU tr) (tINormalDiscNu tr)

        es = do
            a <- innNAs tr

            (++) 
                (do
                   d1 <- asList $ iNormalDiscsContainingNormalArc (innNA_fst a) 
                   guard (hasNonzeroCoeffOrIsTri d1)
                   return (Right d1, Left a, (d1,a))
                   )
                (do
                   d2 <- asList $ iNormalDiscsContainingNormalArc (innNA_snd a) 
                   guard (hasNonzeroCoeffOrIsTri d2)
                   return (Left a, Right d2, (d2,a))
                )

    in
        mkGraphWithNu nu ns es


nsg_toDot gr me = 
       -- bendMultiedges' $ 
       graphToDot params gr
    where
        params = nonClusteredParams {

            isDirected = True,

            globalAttributes = [
                GraphAttrs gattrs, 
                EdgeAttrs eattrs,
                NodeAttrs nattrs 
              ],

            fmtNode = fmtNode_,

            fmtEdge = fmtEdge_ 
       
        }

        gattrs = [ Layout "neato" 
                 , FontSize 10 
--                 , Overlap RemoveOverlaps
--                 , Splines SplineEdges
                 , d2t (FigPreamble (backslashPlaceholder `mappend` "small"))
                 , d2t (DocPreamble (
                    mconcat [
                            backslashPlaceholder
                        ,   "usepackage[a2paper]{geometry}"
                        ,   backslashPlaceholder
                        ,   "input{/usr/local/share/tikztri.tex}"
                        ]))


                 ]


        eattrs = [ 
--                    Concentrate True
                    
                        Dir NoDir
                 ]

        nattrs = [ 
                     Margin (DVal 0)
                 ]

        fmtNode_ (_,no) = 
            case no of
                    -- arc
                 Left inna -> 
                        [ 
                            Label (StrLabel "")
                        ,   style "purple,fill=purple"
                        ]
                    -- disc
                 Right nd ->

                     eitherIND
                        (\t -> 
                            [   Shape Triangle
                            ,   mathLabel t
                            ,   style "red"
                            ])
                        (\q ->
                            [   Shape DiamondShape
                            ,   mathLabel (quad_coefficient me q)
                            ])
                        nd
                    


        fmtEdge_ (_,_,(d,a)) =

            let s c = mconcat [ "triangle path=0.45ex, fill,", c]
            in

            if isTri d 
               then 
                    [
                        Len 1
                    ,
                        style $ s "red"
                    , 
                        Weight 5
                    ]

                else 
                    [ 
                        Len 2.5
                    ,
                        style $ s "black"
                    ,
                        Weight 0.1
                    ]


    


                

                
nsg_tr tr me = nsg_toDot (idGr $ normalStuffGraph tr me) me


test_tr tr i = viewDot (nsg_tr tr (qMatchingEquationsInteger tr !! i))
test = test_tr tr_l31


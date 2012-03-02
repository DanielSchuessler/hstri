{-# LANGUAGE FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
import DotUtil
import VerboseDD
import VerboseDD.Types
import Data.Graph.Inductive.Graph
import Util
import Element
import qualified Data.Vector.Generic as VG
import GraphUtil
import ClosedOrCensus6
import ClosedOrCensus7To10
import Control.Category((>>>))
import Data.Lens.Common
import ClosedNorCensus8
import Data.Text.Lazy(pack)
import Data.List
import Numeric
import Data.Graph.Inductive.Query.DFS
import qualified Data.Set as S
import Data.Set(Set)
import DataDeps


g2d gr = 
    modL (graphStatementsL >>> subGraphsL) (map (setL isClusterL False))
    (
    graphToDot 
        defaultParams {
                globalAttributes = 
                    let sz = 0.03 
                    in
                [
                    

                    GraphAttrs [
                        NodeSep 0.03,
                        RankSep [ 0.3 ],
                        UnknownAttribute "d2tgraphstyle" 
                            (pack 
                                (intercalate ","
                                    ["rotate=-90"
                                    ,"every node/.style={inner sep="++showFFloat Nothing (sz/2) ""++"in}"
                                    ]))
                    ],

                    NodeAttrs [
                            Label (StrLabel ""),
                            Margin (PVal (createPoint 0 0)),
                            Shape Circle,
                            Width sz,
                            Height sz
                        ],

                    EdgeAttrs [
                        Concentrate True

                    ]

                ],

                clusterBy = 
                    \nnl@(_,nl) -> C (fst nl) (N nnl),

                clusterID = Int,

                fmtCluster = 
                    \i ->
                        [ GraphAttrs [ Rank SameRank, toLabel (show i) ] 
                        ],

                fmtNode = \(_,(_,nodeType)) -> 

                        case nodeType of 
                           Result -> 
                                [ 
                                    FillColor (X11Color Green),
                                    Style [SItem Filled []]
                                ] 
                           RegularNode -> []


            }
            gr
        )

go = viewDot . g2d . idPatGr . ddrToGraph . dd

main = go (closedOrCensus7To10 !! 14000 )

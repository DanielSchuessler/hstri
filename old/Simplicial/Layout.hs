{-# LANGUAGE TypeOperators, GADTs, ScopedTypeVariables, FlexibleContexts, TupleSections, NoMonomorphismRestriction #-}
module Simplicial.Layout where

import Simplicial.DeltaSet
import GraphUtil
import Data.GraphViz
import Data.GraphViz.Attributes.Colors
import Data.GraphViz.Printing(printIt)
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Commands.IO
import Data.IntMap
import Data.Graph.Inductive
import Data.List
import Data.Vect.Double
import Data.Text.Lazy.IO as Text
import Control.Applicative
import Control.Arrow
import HomogenousTuples
import Control.Monad
import Simplicial.AnySimplex
import Simplicial.DeltaSet2

data LayoutMode n where
    LayoutDebug :: LayoutMode (NodeType,String)
    LayoutNoDebug :: LayoutMode ()

data NodeType = VertNode | TriNode

layout = layoutG LayoutNoDebug

layoutDebug = layoutG LayoutDebug

layoutG :: forall a nl. (ShowN a, Show (a N0), Show (a N2)) => 
            LayoutMode nl -> DeltaSet a -> IO (a N0 -> Vec3)
layoutG mode ds = do
    let 
        oneSkeleton_ :: Gr nl ()
        oneSkeleton_ = nmap labelNode . emap (const ()) $ oneSkeletonGraph ds
        
        labelNode :: a N0 -> nl
        labelNode v = case mode of
                         LayoutNoDebug -> ()
                         LayoutDebug -> (VertNode,show v) 



        params :: GraphvizParams Node nl () () nl
        params = nonClusteredParams {
            isDirected = True,
            globalAttributes = [GraphAttrs
                                [ Dimen 3, 
                                  Dim 3 
                                ]
                               ],

            fmtNode = case mode of
                           LayoutNoDebug -> const []
                           LayoutDebug ->
                               \(_,(nl,str)) ->
                                   [ --toLabel str
                                   ]
                                   ++
                                    case nl of        
                                        VertNode -> [ color Blue ]
                                        TriNode -> [ Shape Triangle ]
          }

    let
         triNodes :: [Context nl ()]
         triNodes = do
             tri <- simps ds n2
             let vs = faces20 ds tri
             return ([],
                     nodeMapGet ds tri,
                     case mode of
                         LayoutNoDebug -> ()
                         LayoutDebug -> (TriNode,show tri),
                          
                     toList3 $ map3 (((),) . nodeMapGet ds) vs)
                     


         withTriNodes = foldr (&) oneSkeleton_ triNodes
                        


    let
        dot = graphToDot params withTriNodes

    Text.writeFile "/tmp/deltaset.dot" (printIt dot) 

    dg <- graphvizWithHandle Neato dot DotOutput hGetDot
            :: IO (DotGraph Node)

    --Text.putStrLn $ printIt dg

    --augmentedOneSkeleton <- graphToGraphWithCmd Neato params oneSkeleton_
    
    let isVertexNode node = case lab (faceGraph ds) node of
                                Just a -> elimAnySimplexWithNat a (\n _ -> caseNat n True (const False))

    let rawCoords :: [(Node,Vec3)] 
        rawCoords = do
        DotNode node attrs <- nodeStmts . graphStatements $ dg
        guard (isVertexNode node)

        let poss = do
            Pos (PointPos (Point x y (Just z) _)) <- attrs
            return (Vec3 x y z)
            
        case poss of
             pos:_ -> return (node,pos)
             _ -> error ("graphviz returned no position for node "++show node++"; returned attrs are: "++show attrs)

    let
        -- Center and normalize
        center = vecSum (snd <$> rawCoords) &* (recip (genericLength rawCoords))
        centered = second (&- center) <$> rawCoords
        maxnormsqr = maximum (normsqr . snd <$> centered)
        coordMap = fromList (second (&* (2/sqrt maxnormsqr)) <$> centered)

        



    return $ (\v -> coordMap ! (nodeMapGet ds v))
        

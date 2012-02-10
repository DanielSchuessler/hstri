{-# LANGUAGE RecordWildCards, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, TupleSections, FlexibleContexts, ViewPatterns, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS -Wall #-}
-- | 
-- Description : Test
module Tikz.Gen(
    module Tikz.StructureGraphLayout,
    tikzStructureGraph,
    tikzStructureGraphForVertexLink,
    PaperTriangleCorner(..),
    allPaperTriangleCorners,
    allPaperTriangleCorners',
    TikzLoc(..),
    TikzLength,
    TikzStyle,
    regularPolygonLocs,
    noExtraEdgeStyles,
    Way(..),
    tsgAuto,
    StructureGraphLayout(..),
    StructureGraphExtraOptions(..),
    defaultSGEE,
    PtcToVertex(..),



    ) where

import Data.List(intercalate)
import Data.Maybe
import Data.String.Interpolation
import HomogenousTuples
import Latexable
import QuadCoordinates
import Tikz.Base
import Tikz.Preview
import TriangulationCxtObject
import qualified Data.Map as M
import Triangulation.Class
import Tikz.StructureGraph
import Tikz.StructureGraphLayout


data Way = NoQuad
         | QuadGiven (QuadCoordinates Integer) 



noExtraEdgeStyles :: ITriangle -> TikzStyle
noExtraEdgeStyles = const ""

regularPolygonLocs
  :: (AsList xs, Element xs ~ IVertex) => xs -> IVertex -> TikzLoc
regularPolygonLocs v =
    let
        eqvs = asList v
        step = div 360 (length eqvs)
        _assocs = 
            [ (x, Polar (i*step) (scopeUnitLength 1))

                | (x,i) <- zip eqvs [0..] ]


        m = M.fromList _assocs 
    in
        \(x :: IVertex) -> 
            fromMaybe (error ("regularPolygonLocs: invalid vertex: "++show x)) (M.lookup x m)








tikzStructureGraph
  :: (?layout::StructureGraphLayout) =>
     Triangulation -> StructureGraphExtraOptions -> Tikz
tikzStructureGraph tr = 
    tikzStructureGraphForVertices tr (tIVertices tr)

tikzStructureGraphForVertexLink
  :: (?layout::StructureGraphLayout) =>
     T IVertex -> StructureGraphExtraOptions -> Tikz
tikzStructureGraphForVertexLink v = 
    tikzStructureGraphForVertices (getTriangulation v) (preimageListOfVertex v)




--sideAngle = (`mod` 360) . (+30) . (*120)


data StructureGraphExtraOptions = SGEE
    {
        outerScopeStyle :: TikzStyle,
        extraEdgeStyles :: ITriangle -> TikzStyle,
        way :: Way
    }

defaultSGEE :: StructureGraphExtraOptions
defaultSGEE = SGEE "" noExtraEdgeStyles NoQuad

tikzStructureGraphForVertices
  :: (?layout::StructureGraphLayout) =>
     Triangulation -> [IVertex] -> StructureGraphExtraOptions -> Tikz
tikzStructureGraphForVertices 
    (tr :: Triangulation)
    (verts::[IVertex]) 
    SGEE{..}
        =

    let

        edgeDraws :: [Tikz]
        edgeDraws = do
            SGE port1 port2 kind <- structureGraphEdges tr verts 

            let
                ports = (port1, port2)
                (ivertex1,ivertex2) = map2 portNode ports
                (ed1,ed2) = map2 portIEdge ports :: Pair IEdge

                ((side1,o1), (side2,o2)) = map2 portSide ports

                orientation = o1 .*. o2

                (outAngle,inAngle) = map2 sideAngle (side1,side2)

                extraEdgeStyle = ensureComma $    extraEdgeStyles (portTriangle port1) 
                                               ++ extraEdgeStyles (portTriangle port2)

                maybeFlipWarning = 
                    if orientation == NoFlip && kind == GluingEdge 
                                then Just "draw=red,thick" 
                                else Nothing

                toOpts = [str|out=$:outAngle$, in=$:inAngle$$extraEdgeStyle$|]

                drawStyle = intercalate "," (catMaybes [kindStyle,maybeFlipWarning,dir]) 

                kindStyle = case kind of
                                 GluingEdge -> Nothing
                                 EdgeEqualityEdge -> Just "draw=green" 

                (dir,lblNode) = 
                    case (way,kind) of
                         (QuadGiven qc, GluingEdge) -> 
                            let
                                (c1,c2) = 
                                    map2 (      quad_coefficient qc 
                                            .   iNormalQuadByDisjointEdge 
                                            )
                                        (ed1, ed2)

                            in
                                case compare c1 c2 of
                                            EQ -> (Nothing,"") 
                                            LT -> (Just "->"
                                                  , [str|node[] {$$$ toLatex (c2-c1) $$$} |]
                                                  )
                                            GT -> (Just "<-"
                                                  , [str|node[] {$$$ toLatex (c1-c2) $$$} |]
                                                  )

                         _ -> (Nothing,"")
                                

            return [str| 
                    \draw [$drawStyle$] ($nodeName ivertex1$) to[$toOpts$] $lblNode$($nodeName ivertex2$);
                    |]





        mainNodeScope = 
           [str|
            % Graph nodes
            \begin{scope}[$outerScopeStyle$,
                        every node/.style={
                                draw, regular polygon, regular polygon sides=3, inner sep=$innerSep$
                        }]
                \small#ivertex in verts: 
                $goVert ivertex$#
            \end{scope}|]

            where
                -- inner sep for graph nodes
                innerSep 
                    | NoQuad <- way = "0.45em"
                    | otherwise = "0.1em"

                goVert ivertex = [str|\node ($ nodeName ivertex $) at $ theLoc $ {$lbl$};|]
                    where
                        theLoc = renderTikzLoc (loc ivertex)
                        trilbl = [str|$$$ toLatex ivertex $$$|]
                        lbl = case way of
                                   NoQuad -> trilbl 
                                   QuadGiven{} -> trilbl
                        

        nodeCornerLabelScope
            | NoQuad <- way = [str|
                                % Node corner labels
                                \begin{scope}[
                                    every node/.style={
                                        inner sep=0.05em
                                    }]
                                    \scriptsize#ivertex in verts:
                                $nodecorners ivertex$#
                                \end{scope}|]
            | otherwise = ""

                where
                    nodecorners ivertex = 
                        let
                            assocs = 
                                asList $ 
                                    map3 (\s -> (s, ptcToIVertex ivertex s)) allPaperTriangleCorners'  
                            
                            nn = nodeName ivertex
                        in
                            [str|
                                # (c,v) in assocs:
                                    \path ($nn$.center) to node[pos=0.56] {$$$ toLatex v $$$} 
                                          ($nn$.$ptc_toTikz c$);#|]

        _pre = case way of
                   NoQuad -> ""
                   QuadGiven w -> [str|% \[ w = $quad_latex tr w$ \\]|]


        edgeScope = [str|
            % Graph edges
            \begin{scope}[every node/.style={circle,fill=gray!30,inner sep=0.25em}]
            \small
            #edgeDraw in edgeDraws:$edgeDraw$#
            \end{scope}|]

    in
        [str|
            $_pre$
            \begin{tikzpicture}[>=latex,line join=bevel,]

            $mainNodeScope$

            $nodeCornerLabelScope$        

            $edgeScope$

            \end{tikzpicture}|]

nodeName :: IVertex -> TikzNodeName
nodeName (viewI -> (I i x)) = show x ++ show i



tsgAuto :: ToTriangulation t => t -> StructureGraphExtraOptions -> IO ()
tsgAuto (toTriangulation -> tr) opts =
    let
        verts = tIVertices tr
    in
        withAuto tr verts (previewTikz 
            (tikzStructureGraphForVertices tr verts opts))






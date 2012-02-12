{-# LANGUAGE TemplateHaskell, ExistentialQuantification, RecordWildCards, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, TupleSections, FlexibleContexts, ViewPatterns, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
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
    previewAuto,
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
import Tikz.Base
import Tikz.Preview
import TriangulationCxtObject
import qualified Data.Map as M
import Triangulation.Class
import Tikz.StructureGraph
import Tikz.StructureGraphLayout
import QuadCoordinates.Class
import Triangulation.AbstractNeighborhood
import FileLocation
import Control.Monad
import Util
import Data.List
import Data.Monoid(mempty)


data Way = NoQuad
         | forall q. QuadCoords q Integer => QuadGiven q



noExtraEdgeStyles :: ITriangle -> TikzStyle
noExtraEdgeStyles = const mempty

regularPolygonLocs
  :: (AsList xs, Element xs ~ IVertex) => xs -> IVertex -> TikzLoc
regularPolygonLocs v =
    let
        eqvs = asList v
        step = 360 / genericLength eqvs
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
    tikzStructureGraph_restricted tr (tIVertices tr) (edges tr)

tikzStructureGraphForVertexLink
  :: (?layout::StructureGraphLayout) =>
     T IVertex -> StructureGraphExtraOptions -> Tikz
tikzStructureGraphForVertexLink v = 
    tikzStructureGraph_restricted (getTriangulation v) (preimageListOfVertex v) []




--sideAngle = (`mod` 360) . (+30) . (*120)


data StructureGraphExtraOptions = SGEE
    {
        outerScopeStyle :: TikzStyle,
        extraEdgeStyles :: ITriangle -> TikzStyle,
        way :: Way
    }

defaultSGEE :: StructureGraphExtraOptions
defaultSGEE = SGEE mempty noExtraEdgeStyles NoQuad

tikzStructureGraph_restricted
  :: (?layout::StructureGraphLayout) =>
     Triangulation -> [IVertex] -> [TEdge] -> StructureGraphExtraOptions -> Tikz
tikzStructureGraph_restricted 
    (tr :: Triangulation)
    (verts::[IVertex]) 
    eds
    SGEE{..}
        =

    let

        edgeDraws :: [Tikz]
        edgeDraws = do
            SGE port1 port2 kind <- structureGraphEdges tr verts 

            guard (kind /= EdgeEqualityEdge) -- disabled

            let
                ports = (port1, port2)
                (ivertex1,ivertex2) = map2 portNode ports
                (ed1,ed2) = map2 portIEdge ports :: Pair IEdge

                ((side1,o1), (side2,o2)) = map2 portSide ports

                orientation = o1 .*. o2

                (outAngle,inAngle) = map2 sideAngle (side1,side2)

                extraEdgeStyle :: TikzStyle
                extraEdgeStyle = 
                           extraEdgeStyles (portTriangle port1) 
                        ++ extraEdgeStyles (portTriangle port2)
                        ++ do { guard (ivertex1==ivertex2); ["looseness=7"] }

                maybeFlipWarning = 
                    if orientation == NoFlip && kind == GluingEdge 
                                then Just "draw=red,thick" 
                                else Nothing

                toStyle = [str|out=$:outAngle$|] :  [str|in=$:inAngle$|] : extraEdgeStyle

                drawStyle = intercalate "," (catMaybes [kindStyle,maybeFlipWarning,dir]) 

                kindStyle = case kind of
                                 GluingEdge -> Nothing
                                 EdgeEqualityEdge -> Just "draw=green" 

                (dir,lblNode) = 
                    case (way,kind) of
                         (QuadGiven qc, GluingEdge) -> 
                            let
                                (c1,c2) = 
                                    map2 (      quadCount qc 
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
                    \draw [$drawStyle$] ($nodeName ivertex1$) to[$renderTikzStyle toStyle$] $lblNode$($nodeName ivertex2$);
                    |]





        mainNodeScope = 
           [str|
            % Graph nodes
            \begin{scope}[$renderTikzStyle outerScopeStyle$,
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

            $renderEdgeNeighborhoods tr eds$

            \end{tikzpicture}|]

nodeName :: IVertex -> TikzNodeName
nodeName (viewI -> (I i x)) = show x ++ show i



previewAuto :: ToTriangulation t => t -> StructureGraphExtraOptions -> IO ()
previewAuto (toTriangulation -> tr) opts =
    let
        verts = tIVertices tr
        eds = edges tr
    in
        withAuto tr verts eds (previewTikz 
            (tikzStructureGraph_restricted tr verts eds opts))


renderEdgeNeighborhoods
  :: (?layout::StructureGraphLayout) =>
     Triangulation -> [TEdge] -> Tikz
renderEdgeNeighborhoods tr eds = concatMap f eds
    where
        f e = [str|
                \begin{scope}[shift={$_loc$}]
                $renderEdgeNeighborhood tr e$
                \end{scope}
              |]
            where
                _loc = renderTikzLoc $ edgeLoc_ ?layout e



renderEdgeNeighborhood :: Triangulation -> TEdge -> Tikz
renderEdgeNeighborhood tr e =
    case someEdgeNeighborhood tr e of 
        Left _ -> $err' "renderEdgeNeighborhood: boundary edge not supported"

        Right (ien_toList -> ients) -> 
            let
                n = length ients
                n' = 360/fi n
                n'' = (180-n')/2

                radius :: Double
                radius = case n of
                             3 -> 3.5
                             _ -> 3

                _main =
                    flip concatMap (zip ients [0..n-1]) (\(ient,(fi -> i)) ->
                        let
                            ccorner = Polar (i * n')       (show radius)
                            dcorner = Polar ((i+1) * n')   (show radius)


                            a = 
                                simpleNode 
                                    (Polar ((i+0.5)*n') (dist 0 n'))
                                    (mathmode (ient_top ient))
                            b = 
                                simpleNode
                                    (Polar ((i+0.5)*n') (dist 3.3 n'))
                                    (mathmode (ient_bot ient))

                            dist _plus ang = show (250/ang + _plus) ++ "ex"


                            c = 
                                simpleNode
                                    (DistanceRot ccorner (dist 0 n'') (-n''/2) tikzOrigin)
                                    (mathmode (ient_left ient))

                            d = 
                                simpleNode
                                    (DistanceRot dcorner (dist 0 n'') (n''/2) tikzOrigin)
                                    (mathmode (ient_right ient))

                        in
                            [str|
                                \draw $renderTikzLoc ccorner$ -- $renderTikzLoc dcorner$ -- (0,0); 
                            |]
                            ++ concatMap renderNode [a,b,c,d]
                    )

            in
                [str|
                        \begin{scope}
                        $_main$
                        \end{scope} |]


simpleNode :: TikzLoc -> Tikz -> TikzNode
simpleNode = TikzNode ""

data TikzNode = TikzNode {
    n_style :: String,
    n_loc :: TikzLoc,
    n_lbl :: Tikz
}
            
renderNode :: TikzNode -> [Char]
renderNode TikzNode{..} = [str| \node [$n_style$] at $renderTikzLoc n_loc$ {$n_lbl$}; 
|]


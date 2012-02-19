{-# LANGUAGE TemplateHaskell, ExistentialQuantification, RecordWildCards, TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving, TupleSections, FlexibleContexts, ViewPatterns, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
-- | 
-- Description : Test
module Tikz.Gen(
    module Tikz.StructureGraphLayout,
    tikzStructureGraph,
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
    previewAutoEns,
    previewAutoFor,
    StructureGraphLayout(..),
    StructureGraphExtraOptions(..),
    defaultSGEE,
    PtcToVertex(..),
    renderEdgeNeighborhood_core



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
import Text.Groom


data Way = NoQuad
         | forall q r. (Latexable r, QuadCoords q r) => QuadGiven q



noExtraEdgeStyles :: ITriangle -> TikzStyle
noExtraEdgeStyles = const mempty

regularPolygonLocs
  :: (AsList xs, Element xs ~ IVertex) => xs -> IVertex -> TikzLoc
regularPolygonLocs v =
    let
        eqvs = asList v
        step = 360 / genericLength eqvs
        _assocs = 
            [ (x, XYPolar (i*step) 1)

                | (x,i) <- zip eqvs [0..] ]


        m = M.fromList _assocs 
    in
        \(x :: IVertex) -> 
            fromMaybe (error ("regularPolygonLocs: invalid vertex: "++show x)) (M.lookup x m)









--sideAngle = (`mod` 360) . (+30) . (*120)


data StructureGraphExtraOptions = SGEE
    {
        outerScopeStyle :: TikzStyle,
        extraEdgeStyles :: ITriangle -> TikzStyle,
        way :: Way
    }

defaultSGEE :: StructureGraphExtraOptions
defaultSGEE = SGEE mempty noExtraEdgeStyles NoQuad

tikzStructureGraph
  :: (?layout::StructureGraphLayout) =>
     Triangulation -> StructureGraphExtraOptions -> Tikz
tikzStructureGraph 
    (tr :: Triangulation)
    SGEE{..}
        =

    let
        verts = sgl_verts ?layout
        eds = sgl_eds ?layout

        edgeDraws :: [Tikz]
        edgeDraws = do
            SGE port1 port2 kind <- structureGraphEdges tr verts 

            guard (kind /= EdgeEqualityEdge) -- disabled

            let
                ports = (port1, port2)
                (ivertex1,ivertex2) = map2 portNode ports
                (ed1,ed2) = map2 portIEdge ports :: Pair IEdge

                ((side1,_), (side2,_)) = map2 portSide ports
--                 ((side1,o1), (side2,o2)) = map2 portSide ports
--                 orientation = o1 .*. o2

                outAngle = adjustedSideAngle ivertex1 side1
                inAngle  = adjustedSideAngle ivertex2 side2

                adjustedSideAngle v s = fi (sideAngle s) + nodeRot_ ?layout v  

                extraEdgeStyle :: TikzStyle
                extraEdgeStyle = 
                           extraEdgeStyles (portTriangle port1) 
                        ++ extraEdgeStyles (portTriangle port2)
                        ++ do { guard (ivertex1==ivertex2); ["looseness=7"] }

                maybeFlipWarning = Nothing 
--                     if orientation == NoFlip && kind == GluingEdge 
--                                 then Just "draw=red,thick" 
--                                 else Nothing

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
                                

            return 
                [str| 
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

                goVert ivertex = 
                        renderScope [theshift]
                        [str|\node $renderOptions opts$($ nodeName ivertex $) {$lbl$};|]
                    where
                        opts = rotationAsOpts "shape border rotate" ivertex
                        trilbl = [str|$$$ toLatex ivertex $$$|]
                        lbl = case way of
                                   NoQuad -> trilbl 
                                   QuadGiven{} -> trilbl


                        theshift = "shift={"++renderTikzLoc (loc ivertex)++"}"
                                


        rotationAsOpts optName ivertex = case nodeRot_ ?layout ivertex of
                                      0 -> []
                                      d -> [optName++"="++show d]
                        

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
                            renderScope (rotationAsOpts "rotate" ivertex) $

                            [str|
                                # (c,v) in assocs:
                                    \path ($nn$.center) to node[pos=0.56] {$$$ toLatex v $$$} 
                                          ($nn$.$ptc_toTikz c$);#|]

        _pre = case way of
                   NoQuad -> ""
                   QuadGiven _ -> "" -- [str|% \[ w = $quad_latex tr w$ \\]|]


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

            $renderEdgeNeighborhoods eds$

            \end{tikzpicture}|]

nodeName :: IVertex -> TikzNodeName
nodeName (viewI -> (I i x)) = show x ++ show i



previewAuto :: ToTriangulation t => t -> StructureGraphExtraOptions -> IO ()
previewAuto (toTriangulation -> tr) opts =
    previewAutoFor tr (tIVertices tr) (edges tr) opts

-- | Only edge neighbordhoods
previewAutoEns
  :: ToTriangulation t => t -> StructureGraphExtraOptions -> IO ()
previewAutoEns (toTriangulation -> tr) opts =
    previewAutoFor tr [] (edges tr) opts 

previewAutoFor
  :: ToTriangulation t =>
     t -> [IVertex] -> [TEdge] -> StructureGraphExtraOptions -> IO ()
previewAutoFor (toTriangulation -> tr) verts eds opts =

        withAuto tr verts eds (previewTikz 
            (tikzStructureGraph tr opts))


renderEdgeNeighborhoods
  :: (?layout::StructureGraphLayout) =>
     [TEdge] -> Tikz
renderEdgeNeighborhoods eds = concatMap f eds
    where
        f e = [str|
                \begin{scope}[shift={$_loc$}]
                $renderEdgeNeighborhood e$
                \end{scope}
              |]
            where
                _loc = renderTikzLoc 
                            ($unEitherC 
                                ("renderEdgeNeighborhoods "++groom eds) 
                                (edgeLoc_ ?layout e))



renderEdgeNeighborhood :: TEdge -> Tikz
renderEdgeNeighborhood e = 
          case someEdgeNeighborhood e of 
                 Left x -> renderEdgeNeighborhood_core 1 (f (ben_toList x)) True
                 Right x -> renderEdgeNeighborhood_core 1 (f (ien_toList x)) False    

    where
        f = map (\x -> (ient_top x, ient_bot x, ient_left x, ient_right x))

renderEdgeNeighborhood_core
  :: Latexable a => TikzFactor -- ^ extra radius scaling factor
  -> [Quadruple a] -> Bool -> [Char]
renderEdgeNeighborhood_core extraScale ients isBoundary =

      let
        realn = length ients

        n | isBoundary = realn + 1 
          | realn == 1 = 2
          | otherwise = realn
        centralAngle = 360/fi n
        cdAngle 
            | n == 1 = 70
            | n == 2 = 70
            | otherwise = (180-centralAngle)/2

        radius :: Double
        radius = extraScale * case n of
                        2 -> 1.5
                        3 -> 3.5
                        _ -> 3

        _main =
            flip concatMap (zip ients [0..realn-1]) (\((a_lbl,b_lbl,c_lbl,d_lbl),iint) ->
                let
                    i = fi iint

                    ccorner = XYPolar (i * centralAngle)       radius
                    dcorner = XYPolar ((i+1) * centralAngle)   radius

                    ab_extraDist | realn == 1 = 0.5
                                 | otherwise = 0

                    a = 
                        simpleNode 
                            (CanvasPolar ((i+0.5)*centralAngle) (dist ab_extraDist centralAngle))
                            (mathmode a_lbl)
                    b = 
                        simpleNode
                            (CanvasPolar ((i+0.5)*centralAngle) (dist (ab_extraDist + 3.3) centralAngle))
                            (mathmode b_lbl)

                    dist _plus ang = show (250/ang + _plus) ++ "ex"


                    c = 
                        simpleNode
                            (DistanceRot ccorner (dist 0 cdAngle) (-cdAngle/2) tikzOrigin)
                            (mathmode c_lbl)

                    d = 
                        simpleNode
                            (DistanceRot dcorner (dist 0 cdAngle) (cdAngle/2) tikzOrigin)
                            (mathmode d_lbl)

                    _lines = 
                        (if i==0 && isBoundary
                            then simpleDraw bdryOpts tikzOrigin [TikzLineTo ccorner] 
                            else "")
                        ++
                        simpleDraw bdryOpts ccorner [TikzLineTo dcorner]
                        ++
                        simpleDraw 
                            (if iint == realn-1 && isBoundary then bdryOpts else []) 
                            tikzOrigin [TikzLineTo dcorner]


                in
                    (case (isBoundary,realn) of
                          (False,1) -> deg1stuff radius
                          (False,2) -> if i==0 then deg2stuff radius else mempty
                          _ -> _lines)
                    `mappend`
                    concatMap renderNode [a,b,c,d]
            )

        scopeOpts = renderOptions
                            (case (isBoundary,realn) of
                                  (False,1) -> ["rotate=90"]
                                  (False,2) -> []
                                  _ -> 

                                    -- prettify orientation
                                    if odd n
                                       then ["rotate="++show (centralAngle/4)]
                                       else ["rotate="++show (centralAngle/2)]
                            )

      in

                [str|
                        % Edge neighborhood
                        \begin{scope}$scopeOpts$
                        $_main$
                        \end{scope} |]


bdryOpts :: TikzGraphicsOptions
bdryOpts = ["thick"]



deg1stuff :: TikzFactor -> Tikz
deg1stuff radius =
    let
        r = XYPolar 0 radius
        l = XYPolar 180 radius
        p = 10
        q = 0.7
    in
        "% Degree 1 edge\n"
        `mappend`
        simpleDraw bdryOpts tikzOrigin [ TikzCircle radius ]
        `mappend`
        simpleDraw [] l [ TikzLineTo r ]
        `mappend`
        simpleDraw ["<->"] 
            (XYPolar (180+p) (q*radius)) 
            [ TikzArc ((-180)+p) (-p) (q*radius) (q*radius) ] 



deg2stuff :: TikzFactor -> Tikz
deg2stuff radius =
    let
        r = XYPolar 0 radius
        l = XYPolar 180 radius
    in
        "% Degree 2 edge\n"
        `mappend`
        simpleDraw bdryOpts tikzOrigin [ TikzCircle radius ]
        `mappend`
        simpleDraw [] l [ TikzLineTo r ]

    



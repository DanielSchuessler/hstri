{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, TupleSections, FlexibleContexts, ViewPatterns, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}
{-# LANGUAGE ImplicitParams #-}
module Tikz.Gen(
    tikzStructureGraph,
    tikzStructureGraphForVertexLink,
    PaperTriangleCorner(..),
    allPaperTriangleCorners,
    allPaperTriangleCorners',
    TikzLoc(..),
    TikzLength,
    TikzStyle,
    regularPolygonLocs,
    ptcToIVertex_fromS3s,
    noExtraEdgeStyles,
    Way(..)



    ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List(find)
import Data.List(intercalate)
import Data.Maybe
import Data.String.Interpolation
import HomogenousTuples
import Latexable
import NormalConstants
import QuadCoordinates
import Tikz.Preview
import TriangulationCxtObject
import qualified Data.Map as M
import Util
import qualified Data.Set as S
import Tikz.Base




noExtraEdgeStyles :: ITriangle -> TikzStyle
noExtraEdgeStyles = const ""


regularPolygonLocs
  :: (AsList xs, Element xs ~ IVertex) => xs -> IVertex -> TikzLoc
regularPolygonLocs v =
    let
        eqvs = asList v
        step = div 360 (length eqvs)
        _assocs = 
            [ (x, Polar (i*step) 1)

                | (x,i) <- zip eqvs [0..] ]


        m = M.fromList _assocs 
    in
        \(x :: IVertex) -> 
            fromMaybe (error ("regularPolygonLocs: invalid vertex: "++show x)) (M.lookup x m)






ptcToIVertex_fromS3s
  :: (IVertex -> S3) -> IVertex -> PaperTriangleCorner -> IVertex
ptcToIVertex_fromS3s nodePerm ivertex = ptcToIVertex_fromS3 (nodePerm ivertex) ivertex

ptcToIVertex_fromS3
  :: S3 -> IVertex -> PaperTriangleCorner -> IVertex
ptcToIVertex_fromS3 g ivertex ptc =
    case otherIVerticesInSameTet ivertex *. g of
         (v0,v1,v2) -> case ptc of
                            PTC_N -> v0
                            PTC_SW -> v1
                            PTC_SE -> v2



nodeAssocs ivertex = map3 (\s -> (s, ?ptcToIVertex ivertex s)) allPaperTriangleCorners'  

portToOIEdge (StructureGraphPort ivertex side) = 
    oiEdgeByVertices (map2 (?ptcToIVertex ivertex) (pts_cornersCCW side))

iEdgeToPaperTriangleSide ivertex = oiEdgeToPaperTriangleSide ivertex . toOrderedFace 

oiEdgeToPaperTriangleSide ivertex ed = 
                fromMaybe (assert False undefined) 
                    . listToMaybe 
                    . mapMaybe f 
                    $ allPaperTriangleSides
            where
                f side =

                    (side,) <$>

                    case portToOIEdge 
                            (StructureGraphPort ivertex side) of

                         ws | ws == ed -> Just NoFlip
                            | ws == (ed *. Flip) -> Just Flip
                            | otherwise -> Nothing

data Way = NoQuad
         | QuadGiven (QuadCoordinates Integer) 

tikzStructureGraph
  :: Triangulation
     -> (IVertex -> PaperTriangleCorner -> IVertex)
     -> (IVertex -> TikzLoc)
     -> TikzStyle
     -> (ITriangle -> TikzStyle)
     -> Way
     -> Tikz
tikzStructureGraph tr ptcToIVertex = 
    let ?ptcToIVertex = ptcToIVertex
    in tikzStructureGraphForVertices tr (tIVertices tr)

tikzStructureGraphForVertexLink
  :: TVertex
     -> (IVertex -> PaperTriangleCorner -> IVertex)
     -> (IVertex -> TikzLoc)
     -> TikzStyle
     -> (ITriangle -> TikzStyle)
     -> Way
     -> Tikz
tikzStructureGraphForVertexLink v ptcToIVertex = 
    let ?ptcToIVertex = ptcToIVertex
    in tikzStructureGraphForVertices (getTriangulation v) (preimageListOfVertex v)

data StructureGraphPort = 
    StructureGraphPort {
        portNode :: IVertex,
        portSide :: PaperTriangleSide
    }

data StructureGraphEdge = SGE {
    sge_from,sge_to :: StructureGraphPort,
    sge_kind :: StructureGraphEdgeKind,
    sge_orientationBehaviour :: S2
}
data StructureGraphEdgeKind = GluingEdge 
                            | EdgeEqualityEdge


portTriangle p = joinIVertexAndEdge (portNode p) (portToOIEdge p)

structureGraphEdges tr verts = 
            concatMap (gluingEdges tr) verts 
            ++
            (concatMap (edgeEqualityEdges verts) . nub' . map getTIndex) verts

gluingEdges tr ivertex = do
            side <- allPaperTriangleSides
            let port = StructureGraphPort ivertex side

            let ed = portToOIEdge port
            let tri = joinIVertexAndEdge ivertex ed   
                        -- portTriangle port 

            otri <- maybeToList (lookupGluingOfITriangle tr tri)
            guard (tri<forgetVertexOrder otri)

            let glu = (tri,otri)
            let ivertex2 = gluingMap glu ivertex
                ed2 = gluingMap glu ed

                (side2,orientation) = 
                    oiEdgeToPaperTriangleSide ivertex2 ed2

                port2 = StructureGraphPort ivertex2 side2

            [SGE port port2 GluingEdge orientation] 


edgeEqualityEdges
  :: (?ptcToIVertex::IVertex -> PaperTriangleCorner -> IVertex) =>
     [IVertex] -> TIndex -> [StructureGraphEdge]
edgeEqualityEdges (S.fromList -> verts) = go 
  where
    go tet = do
        ed <- edgeList tet
        let (v0,v1) = vertices . oppositeIEdge $ ed
            (side0,o0) = iEdgeToPaperTriangleSide v0 ed
            (side1,o1) = iEdgeToPaperTriangleSide v1 ed

        guard (S.member v0 verts)
        guard (S.member v1 verts)

        [SGE 
            (StructureGraphPort v0 side0)
            (StructureGraphPort v1 side1)
            EdgeEqualityEdge
            (o0 .*. o1)]

--sideAngle = (`mod` 360) . (+30) . (*120)


tikzStructureGraphForVertices 
    (tr :: Triangulation)
    (verts::[IVertex]) 
    (loc :: IVertex -> TikzLoc)
    (outerScopeStyle :: TikzStyle)
    (extraEdgeStyles :: ITriangle -> TikzStyle) 
    way
        =

    let

        edgeDraws :: [Tikz]
        edgeDraws = do
            SGE port1 port2 kind orientation <- structureGraphEdges tr verts 

            let
                ivertex1 = portNode port1
                ivertex2 = portNode port2

                ed1 = portToOIEdge port1 
                ed2 = portToOIEdge port2 

                outAngle = sideAngle (portSide port1) 
                inAngle = sideAngle (portSide port2) 

                extraEdgeStyle = ensureComma $    extraEdgeStyles (portTriangle port1) 
                                               ++ extraEdgeStyles (portTriangle port2)

                maybeFlipWarning = if orientation == NoFlip 
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
                                            .   forgetVertexOrder)
                                        (ed1,ed2)

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
                    | QuadGiven{} <- way = "0.1em"

                goVert ivertex = [str|\node ($ nodeName ivertex $) at $ renderTikzLoc (loc ivertex) $ {$lbl$};|]
                    where
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
            | QuadGiven{} <- way = ""

                where
                    nodecorners ivertex = 
                        let
                            assocs = asList $ nodeAssocs ivertex
                            nn = nodeName ivertex
                        in
                            [str|
                                # (c,v) in assocs:
                                    \path ($nn$.center) to node[pos=0.56] {$$$ toLatex v $$$} 
                                          ($nn$.$ptc_toTikz c$);#|]

        pre = case way of
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
            $pre$
            \begin{tikzpicture}[>=latex,line join=bevel,]

            $mainNodeScope$

            $nodeCornerLabelScope$        

            $edgeScope$

            \end{tikzpicture}|]

nodeName :: IVertex -> TikzNodeName
nodeName (viewI -> (I i x)) = show x ++ show i

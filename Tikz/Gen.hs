{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, FlexibleContexts, ViewPatterns, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}
module Tikz.Gen where

import TriangulationCxtObject
import Data.String.Interpolation
import qualified Data.Map as M
import Latexable
import HomogenousTuples
import Tikz.Preview
import Control.Applicative
import NormalConstants
import Data.Maybe
import Control.Monad
import Control.Exception
import Data.List(find)
import QuadCoordinates
import Data.List(intercalate)

noExtraEdgeStyles :: ITriangle -> TikzStyle
noExtraEdgeStyles = const ""

data TikzLoc = Polar { tl_degrees :: Int, tl_radius :: TikzLength }

renderTikzLoc (Polar a r) = [str| ($:a$:$:r$) |]

type TikzLength = Int


type TikzNodeName = String

regularPolygonLocs
  :: (Ord (Element xs), AsList xs) => xs -> Element xs -> TikzLoc
regularPolygonLocs v =
    let
        eqvs = asList v
        step = div 360 (length eqvs)
        _assocs = 
            [ (x, Polar (i*step) 1)

                | (x,i) <- zip eqvs [0..] ]


        m = M.fromList _assocs 
    in
        (m M.!)


newtype TikzRegularPolygonSideIx = TRPSI Int
    deriving(Eq,Num,Ord,Real,Enum,Integral)

instance Show TikzRegularPolygonSideIx where show (TRPSI i) = show i


type TikzStyle = String

ensureComma "" = ""
ensureComma s@(',':_) = s
ensureComma s = ", "++s

nodeVertexOrder
  :: (INormalTri -> S3) -> INormalTri -> Triple IVertex
nodeVertexOrder nodePerm tri =
                (traverseI map3 otherVertices . iNormalTriGetVertex $ tri)
                                *.
                (nodePerm tri :: S3)

sideVertices
  :: (INormalTri -> S3)
     -> INormalTri -> TikzRegularPolygonSideIx -> (IVertex, IVertex)
sideVertices nodePerm ntri (tikzRegularPolygonSideIx :: TikzRegularPolygonSideIx) = 
    case nodeVertexOrder nodePerm ntri of
            (v1,v2,v3) -> 
                case tikzRegularPolygonSideIx of
                    1 -> (v1,v2)
                    2 -> (v2,v3)
                    3 -> (v3,v1)

getTikzRegularPolygonSideIndex
  :: (INormalTri -> S3)
     -> INormalTri
     -> (IVertex, IVertex)
     -> (TikzRegularPolygonSideIx, S2)
getTikzRegularPolygonSideIndex nodePerm ntri us = 
                fromMaybe (assert False undefined) 
                    . listToMaybe 
                    . mapMaybe f 
                    $ [1,2,3]
            where
                f tikzRegularPolygonSideIx =

                    (tikzRegularPolygonSideIx,) <$>

                    case sideVertices nodePerm ntri tikzRegularPolygonSideIx of

                         ws | ws == us -> Just NoFlip
                            | ws == (us *. Flip) -> Just Flip
                            | otherwise -> Nothing

data Way = NoQuad
         | QuadGiven (QuadCoordinates Integer) 

tikzVertexLink 
    (v::TVertex) 
    (loc :: INormalTri -> TikzLoc)
    (nodePerm :: INormalTri -> S3) 
    (outerScopeStyle :: TikzStyle)
    (extraEdgeStyles :: ITriangle -> TikzStyle) 
    way
        =

    let
        tris = vertexLinkingSurfaceTris v
        tr = getTriangulation v





        
                    
        sideAngle = (`mod` 360) . (+30) . (*120)

        edgeDraws :: [Tikz]
        edgeDraws = do
            ntri <- tris
            let ntri_dual = iNormalTriGetVertex ntri
            tikzRegularPolygonSideIx <- [1,2,3]
            let (u,u') = sideVertices nodePerm ntri tikzRegularPolygonSideIx 
            let tri = iTriangleByVertices (ntri_dual, u, u') 

            otri <- maybeToList (lookupGluingOfITriangle tr tri)
            guard (tri<forgetVertexOrder otri)
            let glu = (tri,otri)
            let ntri2 = iNormalTri (gluingMap glu ntri_dual) 
                u2 = gluingMap glu u
                u'2 = gluingMap glu u'

                (tikzRegularPolygonSideIx2,orientation) = 
                    getTikzRegularPolygonSideIndex nodePerm ntri2 (u2, u'2)

                outAngle = sideAngle tikzRegularPolygonSideIx 
                inAngle = sideAngle tikzRegularPolygonSideIx2 

                extraEdgeStyle = ensureComma $    extraEdgeStyles tri 
                                               ++ extraEdgeStyles (forgetVertexOrder otri)

                maybeFlipWarning = if orientation == NoFlip 
                                then Just "draw=red,thick" 
                                else Nothing

                toOpts = [str|out=$:outAngle$, in=$:inAngle$$extraEdgeStyle$|]
                _style = intercalate "," (catMaybes [maybeFlipWarning,dir]) 

                (dir,lblNode) = 
                    case way of
                         NoQuad -> (Nothing,"")
                         QuadGiven qc -> 
                            let
                                (c1,c2) = 
                                    map2 (      quad_coefficient qc 
                                            .   iNormalQuadByDisjointEdge 
                                            .   uncurry iEdgeByVertices)
                                        ((u,u'),(u2,u'2))

                            in
                                case compare c1 c2 of
                                            EQ -> (Nothing,"") 
                                            LT -> (Just "->"
                                                  , [str|node[] {$$$ toLatex (c2-c1) $$$} |]
                                                  )
                                            GT -> (Just "<-"
                                                  , [str|node[] {$$$ toLatex (c1-c2) $$$} |]
                                                  )
                                

            return [str| 
                    \draw [$_style$] ($nodeName ntri$) to[$toOpts$] $lblNode$($nodeName ntri2$);
                    |]





        mainNodeScope = 
           [str|
            % Graph nodes
            \begin{scope}[$outerScopeStyle$,
                        every node/.style={
                                draw, regular polygon, regular polygon sides=3, inner sep=$innerSep$
                        }]
                \small#tri in tris: 
                $goTri tri$#
            \end{scope}|]

            where
                -- inner sep for graph nodes
                innerSep 
                    | NoQuad <- way = "0.45em"
                    | QuadGiven{} <- way = "0.1em"

                goTri tri = [str|\node ($ nodeName tri $) at $ renderTikzLoc (loc tri) $ {$lbl$};|]
                    where
                        trilbl = [str|$$$ toLatex tri $$$|]
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
                                    \scriptsize#tri in tris:
                                $nodecorners tri$#
                                \end{scope}|]
            | QuadGiven{} <- way = ""

                where
                    nodecorners tri = 
                        let
                            vs = asList $ nodeVertexOrder nodePerm tri
                            nn = nodeName tri
                        in
                            [str|
                                # (i,v) in zip [1..] vs: 
                                    \path ($nn$.center) to node[pos=0.56] {$$$ toLatex v $$$} 
                                          ($nn$.corner $:i$);#|]

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

nodeName :: INormalTri -> TikzNodeName
nodeName (iNormalTriGetVertex -> (viewI -> (I i x))) = show x ++ show i

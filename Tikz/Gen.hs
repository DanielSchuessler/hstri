{-# LANGUAGE FlexibleContexts, ViewPatterns, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}
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

test = 
    let 
        v = pMap tr_l31 (0./vA)
        triOrder = flip (./) <$> [ntA,ntC,ntB] <*> [0,1] 

        tikz = go v (regularPolygonLocs triOrder) (const mempty)
    in do
        putStrLn tikz
        previewTikz tikz



type TikzLoc = String
type TikzNodeName = String

regularPolygonLocs
  :: (Ord (Element xs), AsList xs) => xs -> Element xs -> TikzLoc
regularPolygonLocs v =
    let
        eqvs = asList v
        step = div 360 (length eqvs)
        _assocs = 
            [ (x, [str|($: i*step $:1)|])

                | (x,i) <- zip eqvs [0..] ]


        m = M.fromList _assocs 
    in
        (m M.!)




go (v::TVertex) loc (nodePerm :: INormalTri -> S3) =
    let
        tris = vertexLinkingSurfaceTris v
        tr = getTriangulation v

        triStyle = [str|draw, regular polygon, regular polygon sides=3, inner sep=0.3em|]

        nodeVertexOrder tri =
                (traverseI map3 otherVertices . iNormalTriGetVertex $ tri)
                                *.
                nodePerm tri

        nodecorners tri = 
            let
                vs = asList $ nodeVertexOrder tri
                nn = nodeName tri
            in
                [str|# (i,v) in zip [1..] vs: \node at ($nn$.corner $:i$) {$$$ toLatex v $$$};#|]


        edgeDraws = do
            ntri <- tris
            let vs = nodeVertexOrder ntri
            i <- [0,1,2]
            let (u,u') = deleteAt3 i vs
            let tri = iTriangleByVertices (iNormalTriGetVertex ntri, u, u') 
            otri <- maybeToList (lookupGluingOfITriangle tr tri)
            guard (tri<otri)
            undefined



    in
        [str|
    \begin{tikzpicture}[>=latex,line join=bevel,]

        \begin{scope}[x=3cm,y=3cm,shift={(0.5,2)},scale=0.8,
                      every node/.style={
                        $triStyle$
                      }]
            \small#tri in tris: 
            \node ($ nodeName tri $) at $ loc tri $ {$$$ toLatex tri $$$};#
        \end{scope}

        \begin{scope}[every node/.style={fill=white,draw,circle,inner sep=0.1em}]
            \scriptsize#tri in tris:
           $nodecorners tri$#
        \end{scope}

        \begin{scope}[every node/.style={fill=white,draw,circle,inner sep=0.1em}]
#edgeDraw in edgeDraws:
           $edgeDraw$#
        \end{scope}

    \end{tikzpicture}|]

nodeName :: INormalTri -> TikzNodeName
nodeName (iNormalTriGetVertex -> (viewI -> (I i x))) = show x ++ "_" ++ show i

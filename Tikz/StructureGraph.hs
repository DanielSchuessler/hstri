{-# LANGUAGE ViewPatterns, ImplicitParams, RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Tikz.StructureGraph where

import Triangulation
import PrettyUtil
import Util
import Data.Maybe
import Control.Monad
import qualified Data.Set as S
import HomogenousTuples


data StructureGraphPort = 
    StructureGraphPort {
        portNode :: IVertex,
        portEdge :: Edge
    }
    deriving (Eq,Ord,Show)

instance Pretty StructureGraphPort where
    pretty (StructureGraphPort a b) = text "Port" <> pretty (a,b)

portIEdge :: StructureGraphPort -> IEdge
portIEdge p = portEdge p `inIOf` portNode p

data StructureGraphEdge = SGE {
    sge_from,sge_to :: StructureGraphPort,
    _sge_kind :: StructureGraphEdgeKind
}
    deriving Eq
    
instance Show StructureGraphEdge where
    show (SGE a b c) = show (a,b,c)

instance Pretty StructureGraphEdge where
    pretty (SGE a b c) = pretty (a,b,c)

data StructureGraphEdgeKind = GluingEdge 
                            | EdgeEqualityEdge
                            deriving (Eq,Show)

instance Pretty StructureGraphEdgeKind where prettyPrec = prettyPrecFromShow

structureGraphEdges :: Triangulation -> [IVertex] -> [StructureGraphEdge]
structureGraphEdges tr verts = 
            (concatMap (edgeEqualityEdges verts) . nub' . map getTIndex) verts ++
            concatMap (gluingEdges tr) verts 



gluingEdges :: Triangulation -> IVertex -> [StructureGraphEdge]
gluingEdges tr ivertex = do
            ed <- (edgeList . triangleByDualVertex . forgetTIndex) ivertex
            let port1 = StructureGraphPort ivertex ed

            let tri = joinIVertexAndEdge ivertex ed   
                        -- portTriangle port 

            otri <- maybeToList (lookupGluingOfITriangle tr tri)
--             $(traceExps "gluingEdges" ['tri,'otri]) return ()
            guard (tri<forgetVertexOrder otri)

            let glu = (tri,otri)
            let ivertex2 = gluingMap glu ivertex
                ed2 = gluingMap glu ed

                port2 = StructureGraphPort ivertex2 ed2

            [SGE port1 port2 GluingEdge] 


edgeEqualityEdges :: [IVertex] -> TIndex -> [StructureGraphEdge]
edgeEqualityEdges (S.fromList -> verts) = go 
  where
    go tet = do
        ed <- allEdges
        let (v0,v1) = vertices . oppositeEdge $ ed

        mapM_ (\v -> guard (S.member (tet./v) verts)) [v0,v1]

        [SGE 
            (StructureGraphPort (tet./v0) ed)
            (StructureGraphPort (tet./v1) ed)
            EdgeEqualityEdge
            ]

structureGraphPortsForVertex
  :: IVertex -> Triple StructureGraphPort
structureGraphPortsForVertex ivertex =
    map3 (StructureGraphPort ivertex)
         ((edges . triangleByDualVertex . forgetTIndex) ivertex)



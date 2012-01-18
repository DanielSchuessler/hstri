{-# LANGUAGE Rank2Types, OverloadedStrings, ScopedTypeVariables, TupleSections, GeneralizedNewtypeDeriving, ViewPatterns, ImplicitParams, RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Tikz.StructureGraphLayout where

import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import Data.Graph.Inductive.Graph hiding(Edge,edges)
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Maybe
import DotUtil
import GraphUtil
import HomogenousTuples
import Data.Numbering
import Tikz.Base
import TriangulationCxtObject
import qualified Data.Map as M
import Data.Vect.Double.Base(Vec2(..))
import MathUtil
import Data.Ord
import Data.Vect.Double.Base(_2)
import Data.Vect.Double.Base(_1)
import Data.Map(Map)
import qualified Data.Vector as V
import Orientation
import Tikz.StructureGraph

data StructureGraphLayout0 = SGL0 {
    ptcToVertex0 :: Map IVertex PtcToVertex,
    loc0 :: Map IVertex TikzLoc
}

sglToSgl0
  :: [IVertex] -> StructureGraphLayout -> StructureGraphLayout0
sglToSgl0 verts SGL{..} = SGL0
    (M.fromList $ map (id &&& ptcToVertex_) verts)
    (M.fromList $ map (id &&& loc_) verts)

sgl0ToSgl :: StructureGraphLayout0 -> StructureGraphLayout
sgl0ToSgl SGL0{..} = SGL 
    (fromMaybe (assert False undefined) . flip M.lookup ptcToVertex0)
    (fromMaybe (assert False undefined) . flip M.lookup loc0)

data StructureGraphLayout = SGL {
    ptcToVertex_ :: IVertex -> PtcToVertex,
    loc_ :: IVertex -> TikzLoc
}


ptcToVertex
  :: (?layout::StructureGraphLayout) =>
     IVertex -> PaperTriangleCorner -> Vertex
ptcToVertex = applyPtcToVertex . ptcToVertex_ ?layout

loc :: (?layout::StructureGraphLayout) => IVertex -> TikzLoc
loc = loc_ ?layout

portTriangle :: StructureGraphPort -> ITriangle
portTriangle p = joinIVertexAndEdge (portNode p) (portEdge p)

ptcToIVertex_fromS3s :: (IVertex -> S3) -> IVertex -> PtcToVertex
ptcToIVertex_fromS3s nodePerm ivertex = ptcToVertex_fromS3 (nodePerm ivertex) ivertex

ptcToVertex_fromS3 :: S3 -> IVertex -> PtcToVertex
ptcToVertex_fromS3 (g :: S3) ivertex =
    PtcToVertex (otherVertices (forgetTIndex ivertex) *. g)


newtype PtcToVertex = PtcToVertex { 
    -- | Images of 'PTC_N','PTC_SW','PTC_SE'
    ptcToVertex_toTriple :: Triple Vertex 
}
    deriving(RightAction S3)

applyPtcToVertex :: PtcToVertex -> PaperTriangleCorner -> Vertex
applyPtcToVertex (PtcToVertex (v0,v1,v2)) ptc =
        case ptc of
                            PTC_N -> v0
                            PTC_SW -> v1
                            PTC_SE -> v2  


ptcToVertex_fromFunction
  :: (PaperTriangleCorner -> Vertex) -> PtcToVertex
ptcToVertex_fromFunction f = PtcToVertex (f PTC_N, f PTC_SW, f PTC_SE)

portSide
  :: (?layout::StructureGraphLayout) =>
     StructureGraphPort -> (PaperTriangleSide, S2)
portSide (StructureGraphPort ivertex ed) = 
    portSide0 (ptcToVertex_ ?layout ivertex) ed


portSide0 :: PtcToVertex -> Edge -> (PaperTriangleSide, S2)
portSide0 ptcToVertex0 (toOrderedFace -> ed) = 
                fromMaybe (assert False undefined) 
                    . listToMaybe 
                    . mapMaybe f 
                    $ allPaperTriangleSides
            where
                f side =

                    (side,) <$>

                    case verticesToOEdge 
                            (map2 (applyPtcToVertex ptcToVertex0) (pts_cornersCCW side))
                                of
                           

                         ws | ws == ed -> Just NoFlip
                            | ws == (ed *. Flip) -> Just Flip
                            | otherwise -> Nothing

ptcToIVertex
  :: (?layout::StructureGraphLayout) =>
     IVertex -> PaperTriangleCorner -> IVertex
ptcToIVertex (ivertex :: IVertex) (ptc :: PaperTriangleCorner) = 
    getTIndex ivertex ./ ptcToVertex ivertex ptc :: IVertex

auto :: Triangulation -> [IVertex] -> IO StructureGraphLayout
auto tr verts = 
    let
        nnu = nuFromList (concatMap (asList . structureGraphPortsForVertex) verts)

        _edges = [(sge_from e, sge_to e, Just e) | e <- structureGraphEdges tr verts ]
                 ++
                 (do
                    v <- verts
                    (p1,p2) <- (asList . subtuples3_2 . structureGraphPortsForVertex) v
                    return (p1,p2,Nothing))

                 


        fgl = idGr $ mkGraphWithNu' nnu _edges 
                
        params = nonClusteredParams {
                    isDirected = True
                  , globalAttributes = 
                        [GraphAttrs [ Layout "neato" 
                                    , Mode KK 
                                    , Splines SplineEdges
                                    --, Overlap RemoveOverlaps
                                    ]
                        ,EdgeAttrs [ Weight 1 
                                   , Dir NoDir
                                   ]
                        ,NodeAttrs [ UnknownAttribute "label" "" 
                                   , UnknownAttribute "width" "0.06" 
                                   , UnknownAttribute "height" "0.06" 
                                   , UnknownAttribute "margin" "0.02" 
                                   , FontSize 8
                                   
                                   ]
                            
                        ]
                  , fmtNode          = 
                     \(_,x) -> 
                        [ toLabel ((show . (portNode)) x) ]

                  , fmtEdge          = 
                     \(_,_,x) -> 
                        case x of
                            Nothing -> 
                                [ Len 0.2 
                                , UnknownAttribute "color" "gray"
                                ]
                            Just (SGE _ _ k) -> 
                              case k of
                                GluingEdge ->
                                    [ Len 1 
                                    ]
                                EdgeEqualityEdge ->
                                    [ Len 2 
                                    , UnknownAttribute "color" "green"
                                    --, UnknownAttribute "style" "dotted"
                                    , Weight 0.3
                                    , PenWidth 0.5
                                    ]

            }


    in do
--         $(traceExps "" ['_edges,'fgl])
--         viewDot' (graphToDot params fgl)
            ag <- graphToGraph params fgl

            let 
                posMap = M.fromList (do
                            (_,(attrs,port)) <- labNodes ag
                            let PointPos p = getPosAttr attrs 
                            [(port,Vec2 (xCoord p) (yCoord p))]


                            )

                pos = fromMaybe (assert False undefined) . flip M.lookup posMap

                _loc ivertex = 
                    case bary3 (map3 pos (structureGraphPortsForVertex ivertex)) of
                         Vec2 x y -> Cartesian (map2 ptLength (x,y))

                _ptcToVertex ivertex =
                    let
                        -- determine which port is south, northwest and northeast
                        as = map3 (id &&& pos) (structureGraphPortsForVertex ivertex)
                        (aS,a1,a2) = sort3By (comparing ((_2) . snd)) as
                        (aNW,aNE) = sort2By (comparing ((_1) . snd)) (a1,a2) 
                    in
                        ptcToVertex_fromFunction (
                        \ptc -> 
                            let
                                -- opposite port to the corner
                                a = case ptc of
                                         PTC_N -> aS
                                         PTC_SE -> aNW
                                         PTC_SW -> aNE
                            in
                                triangleDualVertex
                                    (joinVertexAndEdge
                                        (unI ivertex)
                                        ((portEdge . fst) a))
                                    )
                                

                _ptcToVertexAlt =
                    case orientTriangulation tr of
                        Left _ -> _ptcToVertex
                        Right tOr -> 
                            \ivertex -> PtcToVertex $
                                let 
                                    tri = iTriangleByDualVertex ivertex
                                    vs = vertices (unI tri)
                                in
                                    case tOrGetTriangleOr tOr tri of
                                         AscOr -> vs
                                         FlipAscOr -> vs *. S3bac
                                    
                                    


            --return (optimize tr verts (SGL _ptcToVertex _loc))
            return ((SGL _ptcToVertexAlt _loc))





withAuto :: Triangulation -> [IVertex] -> ((?layout :: StructureGraphLayout) => IO r) -> IO r
withAuto tr verts k = do 
    layout <- auto tr verts 
    let ?layout = layout 
     in k

adjustPtcToVertex
  :: StructureGraphLayout0
     -> IVertex -> (PtcToVertex -> PtcToVertex) -> StructureGraphLayout0
adjustPtcToVertex l v f = 
    l { ptcToVertex0 =  M.adjust f v (ptcToVertex0 l) } 

optimize
  :: Triangulation
     -> [IVertex] -> StructureGraphLayout -> StructureGraphLayout
optimize tr (verts :: [IVertex]) = sgl0ToSgl . go . sglToSgl0 verts
    where
        go = hillClimb (successors <=< successors <=< successors <=< successors) badness 

        _edges = V.fromList (structureGraphEdges tr verts)

        successors l = do
            v <- verts
            map (\g -> l { ptcToVertex0 =  
                                M.adjust (*. g) v (ptcToVertex0 l) }) 
                (asList transpositions)

        badness l = 
            let
                ?layout = sgl0ToSgl l
            in let 
                edgeBadness =
                        (\(SGE port1 port2 k) ->

                            case k of
                                EdgeEqualityEdge -> 0 :: Int
                                GluingEdge ->
                                    let
                                        ports = (port1, port2)
                                        ((_side1,o1), (_side2,o2)) = map2 portSide ports

                                        orientationBehaviour = o1 .*. o2

                                    in
                                        case orientationBehaviour of
                                            Flip -> 0
                                            NoFlip -> 1)


            in V.sum (V.map edgeBadness _edges)


requiredPtcToVertex
  :: PtcToVertex -> StructureGraphPort -> StructureGraphPort -> PtcToVertex
requiredPtcToVertex port1PtcToVertex port1 port2 =
    let
        (_,o1) = portSide0 port1PtcToVertex (portEdge port1) 
        good port2PtcToVertex = 
            let
                (_,o2) = portSide0 port2PtcToVertex (portEdge port2) 
            in
                o1 .*. o2 == Flip

        ptcToVertexCandidateA = ptcToVertex_fromS3 mempty (portNode port1)
        ptcToVertexCandidateB = ptcToVertex_fromS3 S3bac (portNode port1)
    in
        if good ptcToVertexCandidateA
           then ptcToVertexCandidateA
           else assert (good ptcToVertexCandidateB) ptcToVertexCandidateB
    




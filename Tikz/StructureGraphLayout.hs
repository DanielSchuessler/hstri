{-# LANGUAGE TemplateHaskell, Rank2Types, OverloadedStrings, ScopedTypeVariables, TupleSections, GeneralizedNewtypeDeriving, ViewPatterns, ImplicitParams, RecordWildCards #-}
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
import FileLocation
import Util

data StructureGraphLayout = SGL {
    ptcToVertex0 :: Map IVertex PtcToVertex,
    loc0 :: Map IVertex TikzLoc,
    edgeLoc0 :: Map TEdge TikzLoc
}
    deriving Show


ptcToVertex_ :: StructureGraphLayout -> IVertex -> PtcToVertex
ptcToVertex_ = flip $indx . ptcToVertex0
loc_ :: StructureGraphLayout -> IVertex -> TikzLoc
loc_ = flip $indx . loc0
edgeLoc_ :: StructureGraphLayout -> TEdge -> TikzLoc
edgeLoc_ = flip $indx . edgeLoc0




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
    deriving(Show,RightAction S3)

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



fglGraphForAuto
  :: Graph gr =>
     Triangulation
     -> [IVertex]
     -> [TEdge]
     -> gr (Either StructureGraphPort TEdge) (Maybe StructureGraphEdge)
fglGraphForAuto tr verts eds = mkGraphWithNu' nnu _edges 
    where

        nnu = nuFromList (concatMap (asList . structureGraphPortsForVertex) verts)
               `eitherNu`
              nuFromList eds

              

        _edges = [(Left (sge_from e), Left (sge_to e), Just e) | e <- structureGraphEdges tr verts ]
                 ++
                 (do
                    v <- verts
                    (p1,p2) <- (asList . subtuples3_2 . structureGraphPortsForVertex) v
                    return (Left p1,Left p2,Nothing))

                 
dotGraphParamsForAuto
  :: GraphvizParams
       n
       (Either StructureGraphPort TEdge)
       (Maybe StructureGraphEdge)
       ()
       (Either StructureGraphPort TEdge)
dotGraphParamsForAuto =

        nonClusteredParams {
                    isDirected = True
                  , globalAttributes = 
                        [GraphAttrs [ Layout "neato" 
                                    --, Mode KK 
                                    , Splines SplineEdges
                                    --, Overlap RemoveOverlaps
                                    ]
                        ,EdgeAttrs [ Weight 1 
                                   , Dir NoDir
                                   ]
                        ,NodeAttrs [ UnknownAttribute "label" "" 
                                   , FontSize 8
                                   
                                   ]
                            
                        ]
                  , fmtNode          = 
                     \(_,x) -> 
                        case x of 
                             Left sgport -> 
                                [ toLabel ((show . portNode) sgport) 
                                , UnknownAttribute "width" "0.06" 
                                , UnknownAttribute "height" "0.06" 
                                , UnknownAttribute "margin" "0.02" 
                                
                                ]
                             Right _ -> 
                                [
                                  UnknownAttribute "width" "2"
                                , UnknownAttribute "height" "2"
                                , Shape Circle
                                ]


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


vec2InPointsToTikzLoc :: Vec2 -> TikzLoc
vec2InPointsToTikzLoc (Vec2 x y) = Cartesian (map2 ptLength (x,y))

auto :: Triangulation -> [IVertex] -> [TEdge] -> IO StructureGraphLayout
auto tr verts eds = 
    let
        fgl = idGr $ fglGraphForAuto tr verts eds

    in do
--         $(traceExps "" ['_edges,'fgl])
--         viewDot' (graphToDot params fgl)
            ag <- graphToGraph dotGraphParamsForAuto fgl

            let 
                posMap = M.fromList (do
                            (_,(attrs,x)) <- labNodes ag
                            [(x, posToVec2 (getPosAttr attrs))]
                            )

                pos = flip $indx posMap

                _loc ivertex = 
                         vec2InPointsToTikzLoc .
                         bary3 .
                         map3 (pos . Left) .
                         structureGraphPortsForVertex $ ivertex 
                         

                _ptcToVertex ivertex =
                    let
                        -- determine which port is south, northwest and northeast
                        as = map3 (id &&& (pos . Left)) (structureGraphPortsForVertex ivertex)
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


                _edgeLoc = vec2InPointsToTikzLoc . pos . Right
                                    
                                    


            --return (optimize tr verts (SGL _ptcToVertex _loc))
            return 
                (SGL {
                    ptcToVertex0 = funToMap verts _ptcToVertexAlt,
                    loc0 = funToMap verts _loc,
                    edgeLoc0 = funToMap eds _edgeLoc
                })





withAuto :: Triangulation -> [IVertex] -> [TEdge] -> ((?layout :: StructureGraphLayout) => IO r) -> IO r
withAuto tr verts eds k = do 
    layout <- auto tr verts eds 
    let ?layout = layout 
     in k

adjustPtcToVertex
  :: StructureGraphLayout
     -> IVertex -> (PtcToVertex -> PtcToVertex) -> StructureGraphLayout
adjustPtcToVertex l v f = 
    l { ptcToVertex0 =  M.adjust f v (ptcToVertex0 l) } 

optimize
  :: Triangulation
     -> [IVertex] -> StructureGraphLayout -> StructureGraphLayout
optimize tr (verts :: [IVertex]) = go
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
                ?layout = l
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
    




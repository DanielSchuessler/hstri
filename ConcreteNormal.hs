{-# LANGUAGE TemplateHaskell, FlexibleContexts, CPP, RecordWildCards, NoMonomorphismRestriction, FlexibleInstances, StandaloneDeriving, GADTs, ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module ConcreteNormal(
    module StandardCoordinates,
    module SimplicialPartialQuotient,
    module PreRenderable,
    module Simplicial.SimplicialComplex,
    Concrete,c_unique,c_type,
    Corn,
    standardCoordinatesToPreRenderable,

    )
    where

import AbstractTetrahedron
import Control.Exception
import Data.Set as Set
import Data.Vect.Double(interpolate)
import HomogenousTuples
import StandardCoordinates
import PreRenderable
import PrettyUtil
import Simplicial.AnySimplex
import Simplicial.SimplicialComplex
import SimplicialPartialQuotient
import TriangulationCxtObject

-- | Quads are numbered away from their 'firstDisjointEdge'
firstDisjointEdge :: NormalQuad -> Edge
firstDisjointEdge q = uncurry min (normalQuadGetDisjointEdges q)

data Concrete a = C !Int !a
    deriving(Eq,Ord,Show)

instance Pretty a => Pretty (Concrete a) where
    prettyPrec prec (C u a) = prettyPrecApp prec "C" [anyPretty u, anyPretty a] 

c_unique :: Concrete t -> Int
c_unique (C u _) = u
c_type ::  Concrete t -> t
c_type (C _ a) = a

type ArcPosition = Int
type CornerPosition = Int

data ConcreteNormalSurface = CNS {
    cns_tris :: [Concrete INormalTri],
    cns_quads :: [Concrete INormalQuad],

    cns_arcsOfTri :: Concrete INormalTri -> Triple (Concrete INormalArc),
    cns_arcsOfQuad :: Concrete INormalQuad -> Quadruple (Concrete INormalArc),
    cns_cornersOfArc :: Concrete INormalArc -> Pair (Concrete INormalCorner)
}


discPosToCornerPos_helper
  :: (Link NormalCorner disc (Pair NormalCorner),
      HasTIndex idisc disc,
      Show idisc,
      Integral i) =>

     (StandardCoordinates i -> Concrete idisc -> NormalArc -> Int)
     -> StandardCoordinates i
     -> Concrete idisc
     -> NormalCorner
     -> (Pair NormalCorner -> NormalCorner)
     -> CornerPosition
discPosToCornerPos_helper cns_x_arcPos nc x corner linkCornerSelector =
    let
        I i x' = viewI (c_type x)
        arc = normalArc (corner, linkCornerSelector (link corner x'))
        arcPos = cns_x_arcPos nc x arc
        result = arcPosToCornerPos nc (C arcPos (i ./ arc)) corner
    in
--         trace (unwords ["discPosToCornerPos_helper _ _ _",
--                             showsPrec 11 x "",
--                             show corner,
--                             "_",
--                             ":",
--                             $(showVars ['arc,'arcPos,'result])])

              result
         
triPosToCornerPos_helper
  :: Integral i =>
        StandardCoordinates i
     -> Concrete INormalTri
     -> NormalCorner
     -> (Pair NormalCorner -> NormalCorner)
     -> CornerPosition
triPosToCornerPos_helper = discPosToCornerPos_helper triPosToArcPos

quadPosToCornerPos_helper
  :: Integral i =>
        StandardCoordinates i
     -> Concrete INormalQuad
     -> NormalCorner
     -> (Pair NormalCorner -> NormalCorner)
     -> CornerPosition
quadPosToCornerPos_helper = discPosToCornerPos_helper quadPosToArcPos

triPosToCornerPos
  :: Integral i =>
        StandardCoordinates i
     -> Concrete INormalTri
     -> NormalCorner
     -> CornerPosition
triPosToCornerPos nc x c = assert (r1==r2) $ r1
  where
    (r1,r2) = map2 (triPosToCornerPos_helper nc x c) (fst,snd)

quadPosToCornerPos
  :: Integral i =>
        StandardCoordinates i
     -> Concrete INormalQuad
     -> NormalCorner
     -> CornerPosition
quadPosToCornerPos nc x c = 
  let
    (r1,r2) = map2 (quadPosToCornerPos_helper nc x c) (fst,snd)
  in
    if (r1==r2)
       then r1
       else error ("quadPosToCornerPos: (x,c,r1,r2) = "++show (x,c,r1,r2))
        


arcPosToCornerPos :: Integral i => StandardCoordinates i -> 
                    Concrete INormalArc -> NormalCorner -> CornerPosition 
arcPosToCornerPos nc (C arcPos arc) corner =
                let
                    icorner = getTIndex arc ./ corner
                    cornPos_max = fi (numberOfCornersOfType nc icorner - 1)
                    sense = getArcNumberingVsCornerNumberingSense (forgetTIndex arc) corner
                    result = 
                        case sense of 
                                        NoFlip -> arcPos
                                        Flip -> cornPos_max - arcPos
                in

--                     trace (unwords ["arcPosToCornerPos",
--                                         $(prVars' ['arcPos,'arc,'corner,'cornPos_max,'sense,'result])])

                          result



triPosToArcPos :: Integral i => StandardCoordinates i -> Concrete INormalTri -> NormalArc -> ArcPosition 
triPosToArcPos _ (C u _) _ = u

quadPosToArcPos :: Integral i => StandardCoordinates i -> 
                        Concrete INormalQuad -> NormalArc -> ArcPosition 
quadPosToArcPos nc (C u quad) arc =
  let
    iarc = getTIndex quad ./ arc
  in

    if isVertexOfEdge (vertex arc) (firstDisjointEdge (forgetTIndex quad)) 

        then
            -- Arc encloses a vertex on the firstDisjointEdge of the quad type.
            -- => Quads are numbered away from that vertex (i.e.: same direction as arcs)
            fi (numberOfTrisContainingArcType nc iarc) + u

        else
            -- Arc encloses a vertex on the other disjointEdge of the quad type.
            -- => Quads are numbered towards that vertex (i.e.: opposite direction than arcs)
            (fi (numberOfArcsOfType nc iarc) - 1) - u


concreteTris
  :: Integral i =>
     Triangulation -> StandardCoordinates i -> [Concrete INormalTri]
concreteTris tr nc = do
            tri <- tINormalTris tr
            u <- [ 0 .. fi (ncCoefficient nc (iNormalDisc tri)-1) ]

            return (C u tri)

concreteQuads
  :: Integral i =>
     Triangulation -> StandardCoordinates i -> [Concrete INormalQuad]
concreteQuads tr nc = do
            quad <- tINormalQuads tr
            u <- [0 .. fi (ncCoefficient nc (iNormalDisc quad)-1) ]

            return (C u quad) 
                    

mkConcrete :: Integral i => Triangulation -> StandardCoordinates i -> ConcreteNormalSurface
mkConcrete (tr :: Triangulation) nc =
    let
        cns_tris = concreteTris tr nc
        cns_quads = concreteQuads tr nc 

        cns_arcsOfTri cnt =
            map3 (\arc -> C (triPosToArcPos nc cnt (forgetTIndex arc)) arc) (normalArcs . c_type $ cnt) 




        cns_arcsOfQuad cnq =
                    map4 
                        (\arc -> C (quadPosToArcPos nc cnq (forgetTIndex arc)) arc)
                        (normalArcs . c_type $ cnq) 


        cns_cornersOfArc cna =
                map2 
                    (\icorner -> 
                            C (arcPosToCornerPos nc cna (forgetTIndex icorner)) icorner)  
                    (normalCorners . c_type $ cna)



    in
        CNS{..}


getArcNumberingVsCornerNumberingSense
  :: NormalArc -> NormalCorner -> S2
getArcNumberingVsCornerNumberingSense narc ncorner = 
    let
        (v0,v1) = vertices $ normalCornerGetContainingEdge ncorner

        v = normalArcGetVertex narc

    in
        if v0==v 
           then NoFlip
           else assert (v1==v) Flip

        


-- | An interior point of an edge of the 'SimplicialPartialQuotient'
data Corn v = Corn CornerPosition Int v v
    deriving(Eq,Ord,Show)

instance Pretty v => Pretty (Corn v) where
    prettyPrec prec (Corn u n v0 v1) = 
        prettyPrecApp prec "Corn" [anyPretty u,anyPretty n,anyPretty v0,anyPretty v1] 


standardCoordinatesToPreRenderable
  :: forall v i. (Integral i, Ord v, Pretty i, Show v) =>
     SPQWithCoords v
     -> StandardCoordinates i
     -> PreRenderable (OTuple (Corn v))
standardCoordinatesToPreRenderable (SPQWithCoords spq coords) nc =  
    let
        cns = mkConcrete (spq_tr spq) nc

        tris :: [Triple (Corn v)]
        tris = do
            cx@(C _ (viewI -> I i x)) <- cns_tris cns 
            let f corner = 
                    mkCorn 
                        (triPosToCornerPos nc cx corner)
                        (i ./ corner)

            [ map3 f (normalCorners x) ]

        quads :: [Quadruple (Corn v)]
        quads = do
            cx@(C _ (viewI -> I i x)) <- cns_quads cns 
            let f corner = 
                    mkCorn 
                        (quadPosToCornerPos nc cx corner)
                        (i ./ corner)

            [ map4 f (normalCorners x) ]


        (sc,quadDiagonals) = fromTrisAndQuads tris quads

        mkCorn :: CornerPosition -> INormalCorner -> Corn v
        mkCorn pos nonCanonicalINormalCorner =
            let
                n = numberOfCornersOfType nc nonCanonicalINormalCorner
                (u0,u1) = map2 (spq_map spq)
                               (vertices (iNormalCornerGetContainingEdge nonCanonicalINormalCorner))
            in
                if u0 <= u1
                   then Corn (fi pos)         (fi n) u0 u1
                   else Corn (fi n - pos - 1) (fi n) u1 u0

        cornerCoords (Corn pos n v0 v1) = 
                interpolate (fi (1+pos) / fi (1+n)) (coords v0) (coords v1)

        
    in

--         case admissible tr nc of
--              Left err -> error ("standardCoordinatesToPreRenderable: Input surface "++show nc
--                                     ++" not admissible: "++err)
--              Right () ->

                (mkPreRenderable
                    (cornerCoords . unOT)
                    sc)

                    { pr_visible = \asi -> elimAnySimplexWithNat asi (\n ->
                            caseNat2 n
                                (const True)
                                (\e -> not (Set.member e quadDiagonals))
                                (const (const True))) 
                    }


--         ds = mkHomogenousDeltaSet
--                 (mkTuply2dFaceFunc 
--                     (\x -> 
--                         case x of
--                             NSTSArc a -> map2 canonCorn (cns_cornersOfArc a)
--                             NSTSQuadDiagonal q -> 
--                                 case cns_arcsOfQuad of
--                                      (
                    
                        
                
                





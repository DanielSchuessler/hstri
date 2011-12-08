{-# LANGUAGE TemplateHaskell, FlexibleContexts, CPP, RecordWildCards, NoMonomorphismRestriction, FlexibleInstances, StandaloneDeriving, GADTs, ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module ConcreteNormal(
    module NormalCoordinates,
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
import Data.Maybe
import Data.Set as Set
import Data.Vect.Double(interpolate)
import HomogenousTuples
import NormalCoordinates
import PreRenderable
import PrettyUtil
import Simplicial.AnySimplex
import Simplicial.SimplicialComplex
import SimplicialPartialQuotient
import THUtil
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
     -> Triangulation
     -> StandardCoordinates i
     -> Concrete idisc
     -> NormalCorner
     -> (Pair NormalCorner -> NormalCorner)
     -> CornerPosition
discPosToCornerPos_helper cns_x_arcPos tr nc x corner linkCornerSelector =
    let
        I i x' = viewI (c_type x)
        arc = normalArc (corner, linkCornerSelector (link corner x'))
        arcPos = cns_x_arcPos nc x arc
        result = arcPosToCornerPos tr nc (C arcPos (i ./ arc)) corner
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
     Triangulation
     -> StandardCoordinates i
     -> Concrete INormalTri
     -> NormalCorner
     -> (Pair NormalCorner -> NormalCorner)
     -> CornerPosition
triPosToCornerPos_helper = discPosToCornerPos_helper triPosToArcPos

quadPosToCornerPos_helper
  :: Integral i =>
     Triangulation
     -> StandardCoordinates i
     -> Concrete INormalQuad
     -> NormalCorner
     -> (Pair NormalCorner -> NormalCorner)
     -> CornerPosition
quadPosToCornerPos_helper = discPosToCornerPos_helper quadPosToArcPos

triPosToCornerPos
  :: Integral i =>
     Triangulation
     -> StandardCoordinates i
     -> Concrete INormalTri
     -> NormalCorner
     -> CornerPosition
triPosToCornerPos tr nc x c = assert (r1==r2) $ r1
  where
    (r1,r2) = map2 (triPosToCornerPos_helper tr nc x c) (fst,snd)

quadPosToCornerPos
  :: Integral i =>
     Triangulation
     -> StandardCoordinates i
     -> Concrete INormalQuad
     -> NormalCorner
     -> CornerPosition
quadPosToCornerPos tr nc x c = 
  let
    (r1,r2) = map2 (quadPosToCornerPos_helper tr nc x c) (fst,snd)
  in
    if (r1==r2)
       then r1
       else error ("quadPosToCornerPos: (x,c,r1,r2) = "++show (x,c,r1,r2))
        


arcPosToCornerPos :: Integral i => Triangulation -> StandardCoordinates i -> 
                    Concrete INormalArc -> NormalCorner -> CornerPosition 
arcPosToCornerPos tr nc (C arcPos arc) corner =
                let
                    icorner = getTIndex arc ./ corner
                    cornPos_max = fi (numberOfCornersOfType nc icorner - 1)
                    sense = getArcNumberingVsCornerNumberingSense tr arc corner
                    result = 
                        case sense of 
                                        NoFlip -> arcPos
                                        Flip -> cornPos_max - arcPos
                in

                    trace (unwords ["arcPosToCornerPos",
                                        $(showVars ['arcPos,'arc,'corner,'cornPos_max,'sense,'result])])

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
                            C (arcPosToCornerPos tr nc cna (forgetTIndex icorner)) icorner)  
                    (normalCorners . c_type $ cna)



    in
        CNS{..}


canonicalOIEdgeForCorner :: Triangulation -> INormalCorner -> OIEdge
canonicalOIEdgeForCorner tr c = 
            canonicalize tr (edgeToOEdge (iNormalCornerGetContainingEdge c))

    where
        edgeToOEdge :: IEdge -> OIEdge
        edgeToOEdge = toOrderedFace
            

getArcNumberingVsCornerNumberingSense
  :: Triangulation -> INormalArc -> NormalCorner -> S2
getArcNumberingVsCornerNumberingSense tr (viewI -> I i narc) ncorner = 
    let
        incorner = i ./ ncorner
        v = normalArcGetVertex narc
        e = normalCornerGetContainingEdge ncorner
        numberingDirectionForArcs = i ./ oedge (v, otherVertex e v) 

    in
        fromJust (getOIEdgeGluingSense tr 
                    numberingDirectionForArcs 
                    (canonicalOIEdgeForCorner tr incorner)) 

        



data Corn = Corn CornerPosition Int (EquivalenceClass INormalCorner) 
    deriving(Eq,Ord,Show)

instance Pretty Corn where
    prettyPrec prec (Corn u n eq) = prettyPrecApp prec "Corn" [anyPretty u,anyPretty n,anyPretty eq] 

standardCoordinatesToPreRenderable
  :: (Integral i, Eq v, Pretty i) =>
     SPQWithCoords v
     -> StandardCoordinates i
     -> PreRenderable (OTuple Corn)
standardCoordinatesToPreRenderable (SPQWithCoords spq coords) nc =  
    let
        tr = spq_tr spq

        cornEqv = spq_INormalCornerEquivalence spq

        cns = mkConcrete (spq_tr spq) nc

        tris :: [Triple Corn]
        tris = do
            cx@(C _ (viewI -> I i x)) <- cns_tris cns 
            let f corner = 
                    mkCorn 
                        (triPosToCornerPos tr nc cx corner)
                        (i ./ corner)

            [ map3 f (normalCorners x) ]

        quads :: [Quadruple Corn]
        quads = do
            cx@(C _ (viewI -> I i x)) <- cns_quads cns 
            let f corner = 
                    mkCorn 
                        (quadPosToCornerPos tr nc cx corner)
                        (i ./ corner)

            [ map4 f (normalCorners x) ]


        (sc,quadDiagonals) = fromTrisAndQuads tris quads

        mkCorn pos nonCanonicalINormalCorner =
            let
                n = numberOfCornersOfType nc nonCanonicalINormalCorner
            in
                Corn (fi pos) (fi n) (eqv_classOf cornEqv nonCanonicalINormalCorner)

        cornerCoords (Corn pos n ec) = 
            let
                 (v0,v1) = map2 
                            (coords . spq_map spq) 
                            
                            (vertices . 
                             iNormalCornerGetContainingEdge . 
                             canonicalRep 
                                {- doesn't matter which rep we take here, since they all
                                   (by definition of cornEqv) get mapped to the same thing
                                   by spq_map -} $ 
                             ec)

--                 the_oedge = toOrderedFace (iNormalCornerGetContainingEdge icorn)
--                 Just sense = 
--                     getOIEdgeGluingSense tr
--                         (canonicalOIEdgeForCorner tr icorn)
--                         the_oedge
-- 
--                 (v0,v1) = map2 (coords . spq_map spq) (vertices (sense .* the_oedge))
            in
                interpolate (fi (1+pos) / fi (1+n)) v0 v1

        
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
                    
                        
                
                





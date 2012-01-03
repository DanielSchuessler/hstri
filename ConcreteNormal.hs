{-# LANGUAGE TemplateHaskell, FlexibleContexts, CPP, RecordWildCards, NoMonomorphismRestriction, FlexibleInstances, StandaloneDeriving, GADTs, ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module ConcreteNormal(
    module StandardCoordinates,
    Concrete,c_unique,c_type,
    concreteTris,
    concreteQuads,
    posOfCornerOfTri,
    posOfCornerOfQuad,
    CornerPosition,ArcPosition,

    cns_arcsOfTri,
    cns_arcsOfQuad,
    cns_cornersOfArc

    )
    where

import Control.Exception
import HomogenousTuples
import StandardCoordinates
import PrettyUtil
import TriangulationCxtObject

-- | Quads are numbered away from their 'firstDisjointEdge'
firstDisjointEdge :: NormalQuad -> Edge
firstDisjointEdge q = uncurry min (normalQuadGetDisjointEdges q)

data Concrete a = Concrete !Int !a
    deriving(Eq,Ord,Show)

instance Pretty a => Pretty (Concrete a) where
    prettyPrec prec (Concrete u a) = prettyPrecApp prec "Concrete" [anyPretty u, anyPretty a] 

c_unique :: Concrete t -> Int
c_unique (Concrete u _) = u
c_type ::  Concrete t -> t
c_type (Concrete _ a) = a

type ArcPosition = Int
type CornerPosition = Int


-- cns_arcsOfDisc :: (IsDiscShape d, HasTIndex id d) => ConcreteNormalSurface -> Concrete id ->  
-- cns_arcsOfDisc cns d = 
--     case isDiscShapeProof :: DiscShape d of
--          Tri -> cns_arcsOfTri

discPosToCornerPos_helper
  :: (Integral i,
      HasTIndex ia a,
      MakeNormalDisc a,
      NormalSurface s i) =>
     (s -> Concrete ia -> NormalArc -> ArcPosition)
     -> s -> Concrete ia -> NormalCorner -> Bool -> CornerPosition
discPosToCornerPos_helper cns_x_arcPos nc x corner takeFst =
    let
        I i x' = viewI (c_type x)
        arc = normalArc (corner, (if takeFst then fst else snd) 
                                        (adjacentNormalCorners corner (normalDisc x')) :: NormalCorner)
        arcPos = cns_x_arcPos nc x arc
        result = posOfCornerOfArc nc (Concrete arcPos (i ./ arc)) corner
    in
--         trace (unwords ["discPosToCornerPos_helper _ _ _",
--                             showsPrec 11 x "",
--                             show corner,
--                             "_",
--                             ":",
--                             $(showVars ['arc,'arcPos,'result])])

              result
         

posOfCornerOfTri
  :: (Integral i, NormalSurface s i) =>
     s -> Concrete INormalTri -> NormalCorner -> CornerPosition
posOfCornerOfTri nc x c = assert (r1==r2) $ r1
  where
    (r1,r2) = map2 (discPosToCornerPos_helper triPosToArcPos nc x c) (True,False)

posOfCornerOfQuad
  :: (Integral i, NormalSurface s i) =>
     s -> Concrete INormalQuad -> NormalCorner -> CornerPosition
posOfCornerOfQuad nc x c = 
  let
    (r1,r2) = map2 (discPosToCornerPos_helper quadPosToArcPos nc x c) (True,False)
  in
    if (r1==r2)
       then r1
       else error ("posOfCornerOfQuad: (x,c,r1,r2) = "++show (x,c,r1,r2))
        


posOfCornerOfArc
  :: (Integral i, NormalSurface s i) =>
     s -> Concrete INormalArc -> NormalCorner -> Int
posOfCornerOfArc nc (Concrete arcPos arc) corner =
                let
                    icorner = getTIndex arc ./ corner
                    cornPos_max = fi (numberOfCornersOfType nc icorner - 1)
                    sense = getArcNumberingVsCornerNumberingSense (forgetTIndex arc) corner
                    result = 
                        case sense of 
                                        NoFlip -> arcPos
                                        Flip -> cornPos_max - arcPos
                in

--                     trace (unwords ["posOfCornerOfArc",
--                                         $(prVars' ['arcPos,'arc,'corner,'cornPos_max,'sense,'result])])

                          result



triPosToArcPos
  :: (Integral i, NormalSurface s i) =>
     s -> Concrete INormalTri -> NormalArc -> ArcPosition
triPosToArcPos _ (Concrete u _) _ = u

quadPosToArcPos
  :: (Integral i, NormalSurface s i) =>
     s -> Concrete INormalQuad -> NormalArc -> ArcPosition
quadPosToArcPos nc (Concrete u quad) arc =
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
  :: (Integral a, NormalSurface s a) =>
     Triangulation -> s -> [Concrete INormalTri]
concreteTris tr ns = do
            tri <- tINormalTris tr
            u <- [ 0 .. fi (triCount ns tri-1) ]

            return (Concrete u tri)

concreteQuads
  :: (Integral a, NormalSurface s a) =>
     Triangulation -> s -> [Concrete INormalQuad]
concreteQuads tr ns = do
            quad <- tINormalQuads tr
            u <- [ 0 .. fi (quadCount ns quad-1) ]

            return (Concrete u quad) 
                    

cns_arcsOfTri
  :: (Integral i, NormalSurface s i) =>
     s -> Concrete INormalTri -> Triple (Concrete INormalArc)
cns_arcsOfTri nc cnt =
            map3 
                (\arc -> 
                    Concrete 
                        (triPosToArcPos nc cnt (forgetTIndex arc)) 
                        arc) 
                (normalArcs . c_type $ cnt) 




cns_arcsOfQuad
  :: (Integral i, NormalSurface s i) =>
     s -> Concrete INormalQuad -> Quadruple (Concrete INormalArc)
cns_arcsOfQuad nc cnq =
                    map4 
                        (\arc -> Concrete (quadPosToArcPos nc cnq (forgetTIndex arc)) arc)
                        (normalArcs . c_type $ cnq) 


cns_cornersOfArc
  :: (Integral i, NormalSurface s i) =>
     s -> Concrete INormalArc -> Pair (Concrete INormalCorner)
cns_cornersOfArc nc cna =
                map2 
                    (\icorner -> 
                            Concrete (posOfCornerOfArc nc cna (forgetTIndex icorner)) icorner)  
                    (normalCorners . c_type $ cna)




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

        



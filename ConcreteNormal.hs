{-# LANGUAGE TemplateHaskell, FlexibleContexts, CPP, RecordWildCards, NoMonomorphismRestriction, FlexibleInstances, StandaloneDeriving, GADTs, ViewPatterns, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module ConcreteNormal(
    module Tetrahedron.NormalDisc,
    module Tetrahedron.NormalConstants,
    module INormalDisc,
    module StandardCoordinates,

    -- * Positions
    Pos,
    CornerPosition,ArcPosition,TriPosition,QuadPosition,
    unPos,

    -- * Concrete normal faces
    Concrete,c_pos,c_type,

    posOfCornerOfTri,
    posOfCornerOfQuad,

    concreteTris,
    concreteQuads,
    concreteArcs,

    concreteArcsOfTri,
    concreteArcsOfQuad,
    concreteCornersOfArc,
    concreteCornersOfTri,
    concreteCornersOfQuad,


    )
    where

import Control.Exception
import HomogenousTuples
import PrettyUtil
import QuadCoordinates.Class
import ShortShow
import StandardCoordinates
import TriangulationCxtObject
import Util
import Tetrahedron.NormalDisc
import Tetrahedron.NormalConstants
import INormalDisc

firstDisjointEdge :: NormalQuad -> Edge
firstDisjointEdge q = uncurry min (normalQuadGetDisjointEdges q)

newtype Pos a = Pos { unPos :: Int }
    deriving(Eq,Ord,Num,Pretty,ShortShow,Enum)


-- | Normal corners are numbered from the smaller vertex to the larger vertex of the edge they're on.
type CornerPosition = Pos INormalCorner

-- | Arcs are numbered away from the vertex they enclose.    
type ArcPosition = Pos INormalArc

-- | Tris are numbered away from the vertex they enclose.
type TriPosition = Pos INormalTri

-- | Quads are numbered away from their 'firstDisjointEdge'.
type QuadPosition = Pos INormalQuad

coercePos :: Pos t -> Pos a
coercePos (Pos i) = Pos i

instance Show (Pos a) where show = show . unPos

data Concrete a = Concrete (AnyStandardCoords Integer) !(Pos a) !a
    deriving(Eq,Ord,Show)

c_pos :: Concrete t -> Pos t
c_pos (Concrete u _) = u

c_type ::  Concrete t -> t
c_type (Concrete _ a) = a

instance Pretty a => Pretty (Concrete a) where
    prettyPrec prec x = 
        prettyPrecApp prec "Concrete" [anyPretty (c_pos x), anyPretty (c_type x)] 




-- cns_arcsOfDisc :: (IsDiscShape d, HasTIndex id d) => ConcreteNormalSurface -> Concrete id ->  
-- cns_arcsOfDisc cns d = 
--     case isDiscShapeProof :: DiscShape d of
--          Tri -> concreteArcsOfTri

discPosToCornerPos_helper
  :: (Integral i,
      HasTIndex ia a,
      MakeNormalDisc a,
      StandardCoords s i) =>
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
  :: (Integral i, StandardCoords s i) =>
     s -> Concrete INormalTri -> NormalCorner -> CornerPosition
posOfCornerOfTri nc x c = assert (r1==r2) $ r1
  where
    (r1,r2) = map2 (discPosToCornerPos_helper posOfArcOfTri nc x c) (True,False)

posOfCornerOfQuad
  :: (Integral i, StandardCoords s i) =>
     s -> Concrete INormalQuad -> NormalCorner -> CornerPosition
posOfCornerOfQuad nc x c = 
  let
    (r1,r2) = map2 (discPosToCornerPos_helper posOfArcOfQuad nc x c) (True,False)
  in
    if (r1==r2)
       then r1
       else error ("posOfCornerOfQuad: (x,c,r1,r2) = "++show (x,c,r1,r2))
        


posOfCornerOfArc
  :: (Integral i, StandardCoords s i) =>
     s -> Concrete INormalArc -> NormalCorner -> CornerPosition
posOfCornerOfArc nc (Concrete (unPos -> arcPos) arc) corner = Pos $
                let
                    icorner = getTIndex arc ./ corner
                    cornPos_max = fi (numberOfCornersOfType nc icorner - 1)
                    sense = getArcNumberingVsCornerNumberingSense (unI arc) corner

                in
                        case sense of 
                                        NoFlip -> arcPos
                                        Flip -> cornPos_max - arcPos



posOfArcOfTri
  :: (Integral i, StandardCoords s i) =>
     s -> Concrete INormalTri -> NormalArc -> ArcPosition
posOfArcOfTri _ x _ = coercePos (c_pos x)

posOfArcOfQuad
  :: (Integral i, StandardCoords s i) =>
     s -> Concrete INormalQuad -> NormalArc -> ArcPosition
posOfArcOfQuad nc cquad arc = Pos $
  let
    u = unPos . c_pos $ cquad 
    quad = c_type cquad
    iarc = getTIndex quad ./ arc
  in

    if isVertexOfEdge 
            (normalArcGetVertex arc) 
            (firstDisjointEdge (unI quad)) 

        then
            -- Arc encloses a vertex on the firstDisjointEdge of the quad type.
            -- => Quads are numbered away from that vertex (i.e.: same direction as arcs)
            fi (numberOfTrisContainingArcType nc iarc) + u

        else
            -- Arc encloses a vertex on the other disjointEdge of the quad type.
            -- => Quads are numbered towards that vertex (i.e.: opposite direction than arcs)
            (fi (numberOfArcsOfType nc iarc) - 1) - u


concreteTris
  :: (Integral a, StandardCoords s a) => s -> [Concrete INormalTri]
concreteTris ns = do
            (x,n) <- triAssocsDistinct ns
            u <- [ 0 .. fi n - 1 ]
            return (Concrete u x) 

concreteQuads
  :: (Integral a, QuadCoords q a) => q -> [Concrete INormalQuad]
concreteQuads ns = do
            (x,n) <- quadAssocsDistinct ns
            u <- [ 0 .. fi n - 1 ]
            return (Concrete u x) 


concreteArcs
  :: (Integral i, StandardCoords a i) => a -> [Concrete INormalArc]
concreteArcs ns = do
    (a,n) <- arcAssocsDistinct ns 
    u <- [0..fi n - 1]
    return (Concrete u a)

    

                    

concreteArcsOfTri
  :: (Integral i, StandardCoords s i) =>
     s -> Concrete INormalTri -> Triple (Concrete INormalArc)
concreteArcsOfTri nc cnt =
            map3 
                (\arc -> Concrete (posOfArcOfTri nc cnt (unI arc)) arc) 
                (normalArcs . c_type $ cnt) 

concreteCornersOfTri
  :: (Integral i, StandardCoords s i) =>
     s -> Concrete INormalTri -> Triple (Concrete INormalCorner)
concreteCornersOfTri nc cnt =
            map3 (\x -> Concrete (posOfCornerOfTri nc cnt (unI x)) x) 
                (normalCorners . c_type $ cnt) 



concreteArcsOfQuad
  :: (Integral i, StandardCoords s i) =>
     s -> Concrete INormalQuad -> Quadruple (Concrete INormalArc)
concreteArcsOfQuad nc cnq =
                    map4 
                        (\arc -> Concrete (posOfArcOfQuad nc cnq (unI arc)) arc)
                        (normalArcs . c_type $ cnq) 

concreteCornersOfQuad
  :: (Integral i, StandardCoords s i) =>
     s -> Concrete INormalQuad -> Quadruple (Concrete INormalCorner)
concreteCornersOfQuad nc cnq =
                    map4 
                        (\x -> Concrete (posOfCornerOfQuad nc cnq (unI x)) x)
                        (normalCorners . c_type $ cnq) 

concreteCornersOfArc
  :: (Integral i, StandardCoords s i) =>
     s -> Concrete INormalArc -> Pair (Concrete INormalCorner)
concreteCornersOfArc nc cna =
                map2 
                    (\icorner -> Concrete (posOfCornerOfArc nc cna (unI icorner)) icorner)  
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


instance TriangulationDSnakeItem (Concrete INormalCorner) where
    canonicalize tr x = 
        let
            e = iNormalCornerGetContainingEdge . c_type $ x
            (e',g) = unpackOrderedFace $ canonicalize tr (toOrderedFace e)

            pos' = case g of
                        NoFlip -> c_pos x
                        Flip -> undefined

        in

            Concrete pos' (iNormalCorner e')

instance TriangulationDSnakeItem (Concrete INormalArc) where
    canonicalize tr x = Concrete (c_pos x) (canonicalize tr (c_type x))

-- | Identity
instance TriangulationDSnakeItem (Concrete INormalTri) where
    canonicalize = const id

-- | Identity
instance TriangulationDSnakeItem (Concrete INormalQuad) where
    canonicalize = const id

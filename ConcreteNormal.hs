{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TemplateHaskell, FlexibleContexts, CPP, RecordWildCards, NoMonomorphismRestriction, FlexibleInstances, StandaloneDeriving, GADTs, ViewPatterns, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fwarn-missing-local-sigs #-}

module ConcreteNormal(
    module Tetrahedron.NormalDisc,
    module Tetrahedron.NormalConstants,
    module Tetrahedron.INormalDisc,
    module StandardCoordinates,

    -- * Positions
    Pos,
    CornerPosition,ArcPosition,TriPosition,QuadPosition,
    unPos,
    posOfCornerOfTri,
    posOfCornerOfQuad,
    coercePos,

    -- * Concrete normal faces
    Concrete(..),c_surf,c_pos,c_type,c_questionableUpdate,
    NmCorner,
    NmArc,
    NmTri,
    NmQuad,


    concreteTris,
    concreteQuads,
    concreteArcs,
    concreteCorners,

    concreteArcsOfTri,
    concreteArcsOfQuad,
    concreteCornersOfArc,
    concreteCornersOfTri,
    concreteCornersOfQuad,

    trisOfSameType,
    quadsOfSameType,
    arcsOfSameType,
    cornersOfSameType,

    canonicalizeConcreteArc,

    -- * Quad halfs
    NmQuadHalf,
    NmTriOrQuadHalf,
    NmAdjoinQuadDiags,

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
import Tetrahedron.INormalDisc
import Control.Arrow
import Data.Function
import StandardCoordinates.MatchingEquations
import Control.Monad
import QuadHalf

firstDisjointEdge :: NormalQuad -> Edge
firstDisjointEdge q = uncurry min (normalQuadGetDisjointEdges q)

newtype Pos a = Pos { unPos :: Int }
    deriving(Eq,Ord,Num,ShortShow,Enum)

instance Pretty (Pos a) where
    pretty = dullyellow . int . unPos

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

instance Show a => Show (Concrete a) where
    showsPrec prec (Concrete _ p t) = 
        showParen (prec>10)
            (showString "Conc " . showsPrec 11 p . showChar ' ' . showsPrec 11 t)

type NmCorner = Concrete INormalCorner
type NmArc = Concrete INormalArc
type NmTri = Concrete INormalTri
type NmQuad = Concrete INormalQuad

c_surf :: Concrete t -> AnyStandardCoords Integer
c_surf (Concrete s _ _) = s

c_pos :: Concrete t -> Pos t
c_pos (Concrete _ u _) = u

c_type ::  Concrete t -> t
c_type (Concrete _ _ a) = a

c_questionableUpdate
  :: (Pos t -> Pos a) -> (t -> a) -> Concrete t -> Concrete a
c_questionableUpdate mapPos mapType (Concrete a b c) = Concrete a (mapPos b) (mapType c)

-- | Assumes the operands belong to the same normal surface; does /not/ compare the surfaces.
instance Eq a => Eq (Concrete a) where
    (==) = (==) `on` (c_pos &&& c_type)

-- | Assumes the operands belong to the same normal surface; does /not/ compare the surfaces.
instance Ord a => Ord (Concrete a) where
    compare = compare `on` (c_pos &&& c_type)

instance Pretty a => Pretty (Concrete a) where
    prettyPrec prec x = 
        prettyPrecApp prec "Concrete" [anyPretty (c_pos x), anyPretty (c_type x)] 

instance ShortShow v => ShortShow (Concrete v) where
    shortShow c = shortShow (c_type c) ++ "#" ++ shortShow (c_pos c)




-- cns_arcsOfDisc :: (IsDiscShape d, HasTIndex id d) => ConcreteNSurface -> Concrete id ->  
-- cns_arcsOfDisc cns d = 
--     case isDiscShapeProof :: DiscShape d of
--          Tri -> concreteArcsOfTri

discPosToCornerPos_helper
  :: (
      HasTIndex ia a,
      MakeNormalDisc a
      ) =>
     (Concrete ia -> NormalArc -> ArcPosition)
     -> Concrete ia -> NormalCorner -> Bool -> CornerPosition
discPosToCornerPos_helper cns_x_arcPos x corner takeFst =
    let
        I i x' = viewI (c_type x)
        arc = normalArc (corner, (if takeFst then fst else snd) 
                                        (adjacentNormalCorners corner (normalDisc x')) :: NormalCorner)
        arcPos = cns_x_arcPos x arc
        result = posOfCornerOfArc (Concrete (c_surf x) arcPos (i ./ arc)) corner
    in
--         trace (unwords ["discPosToCornerPos_helper _ _ _",
--                             showsPrec 11 x "",
--                             show corner,
--                             "_",
--                             ":",
--                             $(showVars ['arc,'arcPos,'result])])

              result
         

posOfCornerOfTri
  :: NmTri -> NormalCorner -> CornerPosition
posOfCornerOfTri x c = assert (r1==r2) $ r1
  where
    (r1,r2) = map2 (discPosToCornerPos_helper posOfArcOfTri x c) (True,False)

posOfCornerOfQuad
  :: NmQuad -> NormalCorner -> CornerPosition
posOfCornerOfQuad x c = 
  let
    (r1,r2) = map2 (discPosToCornerPos_helper posOfArcOfQuad x c) (True,False)
  in
    if (r1==r2)
       then r1
       else error ("posOfCornerOfQuad: (x,c,r1,r2) = "++show (x,c,r1,r2))
        


posOfCornerOfArc
  :: 
     NmArc -> NormalCorner -> CornerPosition
posOfCornerOfArc (Concrete surf (unPos -> arcPos) arc) corner = Pos $
                let
                    icorner = getTIndex arc ./ corner

                    cornPos_max :: Int
                    cornPos_max = fi (numberOfCornersOfType surf icorner - 1)

                    sense = getArcNumberingVsCornerNumberingSense (unI arc) corner

                in
                        case sense of 
                                        NoFlip -> arcPos
                                        Flip -> cornPos_max - arcPos



posOfArcOfTri
  :: NmTri -> NormalArc -> ArcPosition
posOfArcOfTri x _ = coercePos (c_pos x)

posOfArcOfQuad
  :: NmQuad -> NormalArc -> ArcPosition
posOfArcOfQuad cquad arc = Pos $
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
            fi (numberOfTrisContainingArcType (c_surf cquad) iarc) + u

        else
            -- Arc encloses a vertex on the other disjointEdge of the quad type.
            -- => Quads are numbered towards that vertex (i.e.: opposite direction than arcs)
            fi (numberOfArcsOfType (c_surf cquad) iarc) - 1 - u



concreteThings
  :: (Integral r, StandardCoords s Integer) =>
     (Admissible s -> [(a1, r)]) -> Admissible s -> [Concrete a1]
concreteThings thingAssocsDistinct (ns :: Admissible s) = do
            (x,n) <- thingAssocsDistinct ns
            u <- [ 0 .. fi n - 1 ]
            return (Concrete (AnyStandardCoords ns) u x) 

concreteTris
  :: StandardCoords s Integer =>
     Admissible s -> [NmTri]
concreteTris = concreteThings triAssocsDistinct
concreteQuads
  :: StandardCoords s Integer =>
     Admissible s -> [NmQuad]
concreteQuads = concreteThings quadAssocsDistinct
concreteArcs
  :: StandardCoords s Integer =>
     Admissible s -> [NmArc]
concreteArcs = concreteThings arcAssocsDistinct
concreteCorners
  :: StandardCoords s Integer =>
     Admissible s -> [NmCorner]
concreteCorners = concreteThings cornerAssocsDistinct

    

                    
concreteSubSomethings
  :: HasTIndex a a1 =>
     ((a -> Concrete a) -> c -> t)
     -> (b -> c) -> (Concrete b -> a1 -> Pos a) -> Concrete b -> t
concreteSubSomethings mapN somethings posOfSomething x =
            mapN 
                (\sth -> Concrete (c_surf x) (posOfSomething x (unI sth)) sth) 
                (somethings . c_type $ x) 

concreteArcsOfTri :: NmTri -> Triple (NmArc)
concreteArcsOfTri = concreteSubSomethings map3 normalArcs posOfArcOfTri

concreteCornersOfTri :: NmTri -> Triple (NmCorner)
concreteCornersOfTri = concreteSubSomethings map3 normalCorners posOfCornerOfTri

concreteArcsOfQuad :: NmQuad -> Quadruple (NmArc)
concreteArcsOfQuad = concreteSubSomethings map4 iNormalQuadGetNormalArcsInOrder posOfArcOfQuad

concreteCornersOfQuad :: NmQuad -> Quadruple (NmCorner)
concreteCornersOfQuad = concreteSubSomethings map4 iNormalQuadGetNormalCornersInOrder posOfCornerOfQuad

concreteCornersOfArc :: NmArc -> Pair (NmCorner)
concreteCornersOfArc = concreteSubSomethings map2 normalCorners posOfCornerOfArc




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

trisOfSameType :: NmTri -> Integer
trisOfSameType c = triCount (c_surf c) (c_type c)

quadsOfSameType :: NmQuad -> Integer
quadsOfSameType c = quadCount (c_surf c) (c_type c)

arcsOfSameType :: Concrete INormalArc -> Integer
arcsOfSameType c = numberOfArcsOfType (c_surf c) (c_type c)

cornersOfSameType :: Concrete INormalCorner -> Integer
cornersOfSameType c = numberOfCornersOfType (c_surf c) (c_type c)


instance Vertices (NmArc) where
    type Verts (NmArc) = Pair (NmCorner)
    vertices = concreteCornersOfArc

instance Vertices (Concrete OINormalArc) where
    type Verts (Concrete OINormalArc) = Pair (NmCorner)
    vertices = defaultVerticesForOrderedFace

instance Vertices (NmTri) where
    type Verts (NmTri) = Triple (NmCorner)
    vertices = concreteCornersOfTri

instance Vertices (NmQuad) where
    type Verts (NmQuad) = Quadruple (NmCorner)
    vertices = concreteCornersOfQuad

instance Edges (NmTri) where
    type Eds (NmTri) = Triple (NmArc)
    edges = concreteArcsOfTri

instance Edges (NmQuad) where
    type Eds (NmQuad) = Quadruple (NmArc)
    edges = concreteArcsOfQuad

instance RightAction S2 (Concrete OINormalArc) where 
    x *. g = c_questionableUpdate id (*. g) x 

instance OrderableFace (NmArc) (Concrete OINormalArc) where
    type VertexSymGroup (NmArc) = S2

    packOrderedFace x g = c_questionableUpdate coercePos (`packOrderedFace` g) x
    unpackOrderedFace ox =
        let
            (y,g) = unpackOrderedFace (c_type ox)
        in
            (c_questionableUpdate coercePos (const y) ox, g)



type NmQuadHalf = QuadHalf NmQuad
type NmTriOrQuadHalf = TriOrQuadHalf NmTri NmQuad
type NmAdjoinQuadDiags = AdjoinQuadDiags NmArc NmQuad

instance MapTIndices ia => MapTIndices (Concrete ia) where
    mapTIndices f (Concrete s p ia) = Concrete s (coercePos p) (mapTIndices f ia)
    mapTIndicesStrictlyMonotonic f (Concrete s p ia) = Concrete s (coercePos p) (mapTIndicesStrictlyMonotonic f ia)

instance HasTIndex ia a => HasTIndex (Concrete ia) (Concrete a) where
    viewI (Concrete s p (viewI -> (I i a))) = I i (Concrete s (coercePos p) a) 
    i ./ Concrete s p a = Concrete s (coercePos p) (i ./ a)





-- the TriangulationDSnakeItem instances really belong in ConcreteNormal.Identification, but would be orphans

instance TriangulationDSnakeItem NmCorner where
    canonicalize_safe tr x = 
        let
            surf = c_surf x
            e = iNormalCornerGetContainingEdge . c_type $ x
        in
            do
                (e',g) <- fmap unpackOrderedFace 
                            . $commentIfException' "NmCorner/canonicalize_safe: canonicalize_safe for the OIEdge failed" 
                            . canonicalize_safe tr 
                            . toOrderedFace $ e

                let pos' = case g of
                        NoFlip -> c_pos x
                        Flip -> fi (numberOfCornersOfType surf (c_type x)) - c_pos x - 1 

                return (Concrete surf pos' (iNormalCorner e'))

canonicalizeConcreteArc
  :: (Eq t, TriangulationDSnakeItem t, Show t) =>
     Triangulation -> Concrete t -> Concrete t
canonicalizeConcreteArc (tr :: Triangulation) x =
        let
            ina = c_type x
        in
            case canonicalize tr ina of
                ina' | ina == ina' -> x -- redundant; for efficiency(?)
                     | otherwise -> Concrete (c_surf x) (c_pos x) ina' 

instance TriangulationDSnakeItem NmArc where
    canonicalize_safe = (return .) . canonicalizeConcreteArc

instance TriangulationDSnakeItem (Concrete OINormalArc) where
    canonicalize_safe = (return .) . canonicalizeConcreteArc


-- | Identity
instance TriangulationDSnakeItem NmTri where
    canonicalize_safe = const return

-- | Identity
instance TriangulationDSnakeItem NmQuad where
    canonicalize_safe = const return


{-# LANGUAGE TypeFamilies, TemplateHaskell, FlexibleContexts, CPP, RecordWildCards, NoMonomorphismRestriction, FlexibleInstances, StandaloneDeriving, GADTs, ViewPatterns, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

    -- * Concrete normal faces
    Concrete,c_surf,c_pos,c_type,

    concreteTris,
    concreteQuads,
    concreteArcs,
    concreteCorners,

    concreteArcsOfTri,
    concreteArcsOfQuad,
    concreteCornersOfArc,
    concreteCornersOfTri,
    concreteCornersOfQuad,

    -- * Concrete normal surfaces
    TConcrete,
    ConcreteNormalSurface(..),
    toConcrete

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
import Language.Haskell.TH

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
    deriving(Show)


c_surf :: Concrete t -> AnyStandardCoords Integer
c_surf (Concrete s _ _) = s

c_pos :: Concrete t -> Pos t
c_pos (Concrete _ u _) = u

c_type ::  Concrete t -> t
c_type (Concrete _ _ a) = a

-- | Assumes the operands belong to the same normal surface; does /not/ compare the surfaces.
instance Eq a => Eq (Concrete a) where
    (==) = (==) `on` (c_pos &&& c_type)

-- | Assumes the operands belong to the same normal surface; does /not/ compare the surfaces.
instance Ord a => Ord (Concrete a) where
    compare = compare `on` (c_pos &&& c_type)

instance Pretty a => Pretty (Concrete a) where
    prettyPrec prec x = 
        prettyPrecApp prec "Concrete" [anyPretty (c_pos x), anyPretty (c_type x)] 




-- cns_arcsOfDisc :: (IsDiscShape d, HasTIndex id d) => ConcreteNormalSurface -> Concrete id ->  
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
  :: Concrete INormalTri -> NormalCorner -> CornerPosition
posOfCornerOfTri x c = assert (r1==r2) $ r1
  where
    (r1,r2) = map2 (discPosToCornerPos_helper posOfArcOfTri x c) (True,False)

posOfCornerOfQuad
  :: Concrete INormalQuad -> NormalCorner -> CornerPosition
posOfCornerOfQuad x c = 
  let
    (r1,r2) = map2 (discPosToCornerPos_helper posOfArcOfQuad x c) (True,False)
  in
    if (r1==r2)
       then r1
       else error ("posOfCornerOfQuad: (x,c,r1,r2) = "++show (x,c,r1,r2))
        


posOfCornerOfArc
  :: 
     Concrete INormalArc -> NormalCorner -> CornerPosition
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
  :: Concrete INormalTri -> NormalArc -> ArcPosition
posOfArcOfTri x _ = coercePos (c_pos x)

posOfArcOfQuad
  :: Concrete INormalQuad -> NormalArc -> ArcPosition
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
     Admissible s -> [Concrete INormalTri]
concreteTris = concreteThings triAssocsDistinct
concreteQuads
  :: StandardCoords s Integer =>
     Admissible s -> [Concrete INormalQuad]
concreteQuads = concreteThings quadAssocsDistinct
concreteArcs
  :: StandardCoords s Integer =>
     Admissible s -> [Concrete INormalArc]
concreteArcs = concreteThings arcAssocsDistinct
concreteCorners
  :: StandardCoords s Integer =>
     Admissible s -> [Concrete INormalCorner]
concreteCorners = concreteThings cornerAssocsDistinct

    

                    
concreteSubSomethings
  :: HasTIndex a a1 =>
     ((a -> Concrete a) -> c -> t)
     -> (b -> c) -> (Concrete b -> a1 -> Pos a) -> Concrete b -> t
concreteSubSomethings mapN somethings posOfSomething x =
            mapN 
                (\sth -> Concrete (c_surf x) (posOfSomething x (unI sth)) sth) 
                (somethings . c_type $ x) 

concreteArcsOfTri :: Concrete INormalTri -> Triple (Concrete INormalArc)
concreteArcsOfTri = concreteSubSomethings map3 normalArcs posOfArcOfTri

concreteCornersOfTri :: Concrete INormalTri -> Triple (Concrete INormalCorner)
concreteCornersOfTri = concreteSubSomethings map3 normalCorners posOfCornerOfTri

concreteArcsOfQuad :: Concrete INormalQuad -> Quadruple (Concrete INormalArc)
concreteArcsOfQuad = concreteSubSomethings map4 normalArcs posOfArcOfQuad

concreteCornersOfQuad :: Concrete INormalQuad -> Quadruple (Concrete INormalCorner)
concreteCornersOfQuad = concreteSubSomethings map4 normalCorners posOfCornerOfQuad

concreteCornersOfArc :: Concrete INormalArc -> Pair (Concrete INormalCorner)
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


instance TriangulationDSnakeItem (Concrete INormalCorner) where
    canonicalize tr x = 
        let
            surf = c_surf x
            e = iNormalCornerGetContainingEdge . c_type $ x
            (e',g) = unpackOrderedFace . canonicalize tr .  toOrderedFace $ e

            pos' = case g of
                        NoFlip -> c_pos x
                        Flip -> fi (numberOfCornersOfType surf (c_type x)) - c_pos x - 1 

        in

            Concrete surf pos' (iNormalCorner e')

instance TriangulationDSnakeItem (Concrete INormalArc) where
    canonicalize tr x = 
        let
            ina = c_type x
        in
            case canonicalize tr ina of
                ina' | ina == ina' -> x -- redundant; for efficiency(?)
                     | otherwise -> Concrete (c_surf x) (c_pos x) ina' 

-- | Identity
instance TriangulationDSnakeItem (Concrete INormalTri) where
    canonicalize = const id

-- | Identity
instance TriangulationDSnakeItem (Concrete INormalQuad) where
    canonicalize = const id


type TConcrete x = T (Concrete x)

data ConcreteNormalSurface = ConcreteNormalSurface {
    c_corners   :: [ TConcrete INormalCorner   ],
    c_arcs      :: [ TConcrete INormalArc      ],
    c_tris      :: [ TConcrete INormalTri      ],
    c_quads     :: [ TConcrete INormalQuad     ]
 }

-- instance TriangulationDSnakeItem ConcreteNormalSurface where
--     canonicalize tr (ConcreteNormalSurface a b c d)
--         = ConcreteNormalSurface (f a) (f b) (f c) (f d) 
-- 
--             where
--                 f :: TriangulationDSnakeItem a => [a] -> [a]
--                 f = map (canonicalize tr)
            

toConcrete :: forall s. StandardCoords s Integer => Admissible s -> ConcreteNormalSurface
toConcrete s = 
        ConcreteNormalSurface 
            (f concreteCorners) (f concreteArcs)
            (f concreteTris)    (f concreteQuads)

    where
        f :: forall a.
                    TriangulationDSnakeItem a =>
                    (Admissible s -> [a]) -> [T a]

        f g = map (pMap (adm_Triangulation s)) (g s) 
    

instance Vertices (Concrete INormalArc) where
    type Verts (Concrete INormalArc) = Pair (Concrete INormalCorner)
    vertices = concreteCornersOfArc

instance Vertices (TConcrete INormalArc) where
    type Verts (TConcrete INormalArc) = Pair (TConcrete INormalCorner)
    vertices x =
        map2 (pMap (getTriangulation x)) (vertices (unT x))


$(concatMapM (\x -> [d| instance Show (TConcrete $(conT x)) where
                            showsPrec prec = $(varE 'showsPrec {- stage restriction -}) prec . unT |])


             [''INormalDisc,''INormalQuad,''INormalArc,''INormalCorner])



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

    -- * Concrete normal faces
    Concrete,c_surf,c_pos,c_type,
    ConcreteNormalCorner,
    ConcreteNormalArc,
    ConcreteNormalTri,
    ConcreteNormalQuad,


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

    -- * Concrete normal surfaces
    TConcreteNCorner,
    TConcreteNArc,
    TConcreteNTri,
    TConcreteNQuad,
    ConcreteNSurface(..),
    toConcrete,


    -- * Quad halfs
    TConcreteNQuadHalf(..),
    WhichHalf(..),
    NArcOrNQuadDiagonal(..),
    NTriOrNQuadHalf(..)

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
import THBuild
import Control.Monad
import Data.SumType
import Simplicial.DeltaSet1
import DisjointUnion

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

type ConcreteNormalCorner = Concrete INormalCorner
type ConcreteNormalArc = Concrete INormalArc
type ConcreteNormalTri = Concrete INormalTri
type ConcreteNormalQuad = Concrete INormalQuad

c_surf :: Concrete t -> AnyStandardCoords Integer
c_surf (Concrete s _ _) = s

c_pos :: Concrete t -> Pos t
c_pos (Concrete _ u _) = u

c_type ::  Concrete t -> t
c_type (Concrete _ _ a) = a

c_update
  :: (Pos t -> Pos a) -> (t -> a) -> Concrete t -> Concrete a
c_update mapPos mapType (Concrete a b c) = Concrete a (mapPos b) (mapType c)

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
    shortShow c = shortShow (c_type c) ++ shortShow (c_pos c)




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
  :: ConcreteNormalTri -> NormalCorner -> CornerPosition
posOfCornerOfTri x c = assert (r1==r2) $ r1
  where
    (r1,r2) = map2 (discPosToCornerPos_helper posOfArcOfTri x c) (True,False)

posOfCornerOfQuad
  :: ConcreteNormalQuad -> NormalCorner -> CornerPosition
posOfCornerOfQuad x c = 
  let
    (r1,r2) = map2 (discPosToCornerPos_helper posOfArcOfQuad x c) (True,False)
  in
    if (r1==r2)
       then r1
       else error ("posOfCornerOfQuad: (x,c,r1,r2) = "++show (x,c,r1,r2))
        


posOfCornerOfArc
  :: 
     ConcreteNormalArc -> NormalCorner -> CornerPosition
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
  :: ConcreteNormalTri -> NormalArc -> ArcPosition
posOfArcOfTri x _ = coercePos (c_pos x)

posOfArcOfQuad
  :: ConcreteNormalQuad -> NormalArc -> ArcPosition
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
     Admissible s -> [ConcreteNormalTri]
concreteTris = concreteThings triAssocsDistinct
concreteQuads
  :: StandardCoords s Integer =>
     Admissible s -> [ConcreteNormalQuad]
concreteQuads = concreteThings quadAssocsDistinct
concreteArcs
  :: StandardCoords s Integer =>
     Admissible s -> [ConcreteNormalArc]
concreteArcs = concreteThings arcAssocsDistinct
concreteCorners
  :: StandardCoords s Integer =>
     Admissible s -> [ConcreteNormalCorner]
concreteCorners = concreteThings cornerAssocsDistinct

    

                    
concreteSubSomethings
  :: HasTIndex a a1 =>
     ((a -> Concrete a) -> c -> t)
     -> (b -> c) -> (Concrete b -> a1 -> Pos a) -> Concrete b -> t
concreteSubSomethings mapN somethings posOfSomething x =
            mapN 
                (\sth -> Concrete (c_surf x) (posOfSomething x (unI sth)) sth) 
                (somethings . c_type $ x) 

concreteArcsOfTri :: ConcreteNormalTri -> Triple (ConcreteNormalArc)
concreteArcsOfTri = concreteSubSomethings map3 normalArcs posOfArcOfTri

concreteCornersOfTri :: ConcreteNormalTri -> Triple (ConcreteNormalCorner)
concreteCornersOfTri = concreteSubSomethings map3 normalCorners posOfCornerOfTri

concreteArcsOfQuad :: ConcreteNormalQuad -> Quadruple (ConcreteNormalArc)
concreteArcsOfQuad = concreteSubSomethings map4 iNormalQuadGetNormalArcsInOrder posOfArcOfQuad

concreteCornersOfQuad :: ConcreteNormalQuad -> Quadruple (ConcreteNormalCorner)
concreteCornersOfQuad = concreteSubSomethings map4 iNormalQuadGetNormalCornersInOrder posOfCornerOfQuad

concreteCornersOfArc :: ConcreteNormalArc -> Pair (ConcreteNormalCorner)
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


instance TriangulationDSnakeItem (ConcreteNormalCorner) where
    canonicalize_safe tr x = 
        let
            surf = c_surf x
            e = iNormalCornerGetContainingEdge . c_type $ x
        in
            do
                (e',g) <- fmap unpackOrderedFace 
                            . $commentIfException' "ConcreteNormalCorner/canonicalize_safe: canonicalize_safe for the OIEdge failed" 
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

instance TriangulationDSnakeItem (ConcreteNormalArc) where
    canonicalize_safe = (return .) . canonicalizeConcreteArc

instance TriangulationDSnakeItem (Concrete OINormalArc) where
    canonicalize_safe = (return .) . canonicalizeConcreteArc


-- | Identity
instance TriangulationDSnakeItem (ConcreteNormalTri) where
    canonicalize_safe = const return

-- | Identity
instance TriangulationDSnakeItem (ConcreteNormalQuad) where
    canonicalize_safe = const return


type TConcreteNCorner = T (Concrete INormalCorner)
type TConcreteNArc = T (Concrete INormalArc)
type TConcreteNTri = T (Concrete INormalTri)
type TConcreteNQuad = T (Concrete INormalQuad)


data ConcreteNSurface = ConcreteNSurface {
    c_corners   :: [ TConcreteNCorner   ],
    c_arcs      :: [ TConcreteNArc      ],
    c_tris      :: [ TConcreteNTri      ],
    c_quads     :: [ TConcreteNQuad     ]
 }

-- instance TriangulationDSnakeItem ConcreteNSurface where
--     canonicalize tr (ConcreteNSurface a b c d)
--         = ConcreteNSurface (f a) (f b) (f c) (f d) 
-- 
--             where
--                 f :: TriangulationDSnakeItem a => [a] -> [a]
--                 f = map (canonicalize tr)
            

toConcrete :: forall s. StandardCoords s Integer => Admissible s -> ConcreteNSurface
toConcrete s = 
        ConcreteNSurface 
            (f concreteCorners) (f concreteArcs)
            (f concreteTris)    (f concreteQuads)

    where
        f :: forall a.
                    (Show a, TriangulationDSnakeItem a) =>
                    (Admissible s -> [a]) -> [T a]

        f g = map (pMap (adm_Triangulation s)) (g s) 
    

instance Vertices (ConcreteNormalArc) where
    type Verts (ConcreteNormalArc) = Pair (ConcreteNormalCorner)
    vertices = concreteCornersOfArc

instance Vertices (Concrete OINormalArc) where
    type Verts (Concrete OINormalArc) = Pair (ConcreteNormalCorner)
    vertices = defaultVerticesForOrderedFace

instance Vertices (ConcreteNormalTri) where
    type Verts (ConcreteNormalTri) = Triple (ConcreteNormalCorner)
    vertices = concreteCornersOfTri

instance Vertices (ConcreteNormalQuad) where
    type Verts (ConcreteNormalQuad) = Quadruple (ConcreteNormalCorner)
    vertices = concreteCornersOfQuad

instance Edges (ConcreteNormalTri) where
    type Eds (ConcreteNormalTri) = Triple (ConcreteNormalArc)
    edges = concreteArcsOfTri

instance Edges (ConcreteNormalQuad) where
    type Eds (ConcreteNormalQuad) = Quadruple (ConcreteNormalArc)
    edges = concreteArcsOfQuad

instance Vertices TConcreteNArc where
    type Verts TConcreteNArc = Pair (TConcreteNCorner)
    vertices x =
        map2 (pMap (getTriangulation x)) (vertices (unT x))

instance Vertices TConcreteNTri where
    type Verts TConcreteNTri = Triple TConcreteNCorner
    vertices x =
        map3 (pMap (getTriangulation x)) (vertices (unT x))

instance Vertices TConcreteNQuad where
    type Verts TConcreteNQuad = Quadruple TConcreteNCorner
    vertices x =
        map4 (pMap (getTriangulation x)) (vertices (unT x))

instance Edges TConcreteNTri where
    type Eds TConcreteNTri = Triple TConcreteNArc
    edges x =
        map3 (pMap (getTriangulation x)) (edges (unT x))

instance Edges TConcreteNQuad where
    type Eds TConcreteNQuad = Quadruple TConcreteNArc
    edges x =
        map4 (pMap (getTriangulation x)) (edges (unT x))

-- derive stuff for T-types (ignore the triangulation)
$(concatMapM (\((cls,meth),ty) -> 
    sequence [
    sinstanceD (cxt[]) (cls `sappT` (''T `sappT` (''Concrete `sappT` ty))) 
        [svalD meth [| \x -> $(varE meth) x . unT |]]
         
        ]

        )

         (liftM2 (,)     
            [(''Show,'showsPrec),(''Pretty,'prettyPrec),(''ShortShow,'shortShowsPrec)]
            [''INormalTri,''INormalQuad,''INormalArc,''INormalCorner])
             
             
    )

instance RightAction S2 (Concrete OINormalArc) where 
    x *. g = c_update id (*. g) x 

instance OrderableFace (ConcreteNormalArc) (Concrete OINormalArc) where
    type VertexSymGroup (ConcreteNormalArc) = S2

    packOrderedFace x g = c_update coercePos (`packOrderedFace` g) x
    unpackOrderedFace ox =
        let
            (y,g) = unpackOrderedFace (c_type ox)
        in
            (c_update coercePos (const y) ox, g)

data WhichHalf = Half012 | Half023
    deriving (Show,Eq,Ord)

instance ShortShow WhichHalf where
    shortShow = drop 4 . show

instance Pretty WhichHalf where
    pretty = text . show

newtype TConcreteNQuadHalf = CNQH (WhichHalf, TConcreteNQuad) 
    deriving(Show,ShortShow,Eq,Ord,Pretty)

type instance L NArcOrNQuadDiagonal = TConcreteNArc
type instance R NArcOrNQuadDiagonal = TConcreteNQuad

newtype NArcOrNQuadDiagonal = 
        NArcOrNQuadDiagonal (Either TConcreteNArc TConcreteNQuad)
    deriving (Show,ShortShow,Eq,Ord,Pretty,SubSumTy,SuperSumTy)

type instance L NTriOrNQuadHalf = TConcreteNTri
type instance R NTriOrNQuadHalf = TConcreteNQuadHalf

newtype NTriOrNQuadHalf = 
        NTriOrNQuadHalf (Either TConcreteNTri TConcreteNQuadHalf)
    deriving (Show,ShortShow,Eq,Ord,Pretty,SubSumTy,SuperSumTy)

cnqh_quad :: TConcreteNQuadHalf -> TConcreteNQuad
cnqh_quad (CNQH (_,q)) = q

cnqh_which :: TConcreteNQuadHalf -> WhichHalf
cnqh_which (CNQH (w,_)) = w

instance Vertices TConcreteNQuadHalf where
    type Verts TConcreteNQuadHalf = Triple TConcreteNCorner
    vertices qh = case cnqh_which qh of
                       Half012 -> (v0,v1,v2)
                       Half023 -> (v0,v2,v3)
        where
            (v0,v1,v2,v3) = vertices (cnqh_quad qh)


instance Edges TConcreteNQuadHalf where
    type Eds TConcreteNQuadHalf = Triple NArcOrNQuadDiagonal
-- instance Edges CNAO

    edges qh = case cnqh_which qh of
                            -- no particular order
                       Half012 -> (left' e01,left' e12, right' q)
                       Half023 -> (left' e03,left' e23, right' q)
        where
            (e03,e01,e12,e23) = edges q
            q = (cnqh_quad qh)

instance Vertices NArcOrNQuadDiagonal where
    type Verts NArcOrNQuadDiagonal = Pair TConcreteNCorner

    vertices = either' vertices 
        (\q -> 
            let
                (v0,_,v2,_) = vertices q
            in
                (v0,v2))

instance Vertices NTriOrNQuadHalf where
    type Verts NTriOrNQuadHalf = Triple TConcreteNCorner
    vertices = either' vertices vertices

instance Edges NTriOrNQuadHalf where
    type Eds NTriOrNQuadHalf = Triple NArcOrNQuadDiagonal
    edges = either' (map3 left' . edges) edges
                

instance Vertices ConcreteNSurface where
    type Verts ConcreteNSurface = [TConcreteNCorner]
    vertices = c_corners

instance Edges ConcreteNSurface where
    type Eds ConcreteNSurface = [NArcOrNQuadDiagonal]
    edges s = (++) (map left' . c_arcs $ s) (map right' . c_quads $ s)

instance Triangles ConcreteNSurface where
    type Tris ConcreteNSurface = [NTriOrNQuadHalf]
    triangles s = (++)
                    (map left' . c_tris $ s) 
                    [right' (CNQH (w,q))
                        | w <- [Half012,Half023]
                        , q <- c_quads s ]





trisOfSameType :: ConcreteNormalTri -> Integer
trisOfSameType c = triCount (c_surf c) (c_type c)

quadsOfSameType :: ConcreteNormalQuad -> Integer
quadsOfSameType c = quadCount (c_surf c) (c_type c)

arcsOfSameType :: Concrete INormalArc -> Integer
arcsOfSameType c = numberOfArcsOfType (c_surf c) (c_type c)

cornersOfSameType :: Concrete INormalCorner -> Integer
cornersOfSameType c = numberOfCornersOfType (c_surf c) (c_type c)

instance DeltaSet1 ConcreteNSurface

isRegardedAsSimplexByDisjointUnionDeriving ''DIM0 [t|TConcreteNCorner|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM1 [t|TConcreteNArc|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM1 [t|NArcOrNQuadDiagonal|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM2 [t|TConcreteNTri|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM2 [t|TConcreteNQuad|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM2 [t|TConcreteNQuadHalf|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM2 [t|NTriOrNQuadHalf|]

instance MapTIndices ia => MapTIndices (Concrete ia) where
    mapTIndices f (Concrete s p ia) = Concrete s (coercePos p) (mapTIndices f ia)
    mapTIndicesStrictlyMonotonic f (Concrete s p ia) = Concrete s (coercePos p) (mapTIndicesStrictlyMonotonic f ia)

instance HasTIndex ia a => HasTIndex (Concrete ia) (Concrete a) where
    viewI (Concrete s p (viewI -> (I i a))) = I i (Concrete s (coercePos p) a) 
    i ./ Concrete s p a = Concrete s (coercePos p) (i ./ a)



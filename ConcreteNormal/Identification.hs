{-# LANGUAGE TypeFamilies, TemplateHaskell, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses #-}
module ConcreteNormal.Identification where

import ConcreteNormal
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
import Language.Haskell.TH.Build
import Control.Monad
import Data.SumType
import Simplicial.DeltaSet1
import DisjointUnion
import THUtil
import QuadHalf


type TNmCorner = T NmCorner
type TNmArc = T NmArc
type TONmArc = T ONmArc
type TNmTri = T NmTri
type TNmQuad = T NmQuad
type TNmQuadHalf = T NmQuadHalf
type TNmTriOrQuadHalf = T NmTriOrQuadHalf
type TNmAdjoinQuadDiags = T NmAdjoinQuadDiags

type instance L TNmTriOrQuadHalf = TNmTri
type instance R TNmTriOrQuadHalf = TNmQuadHalf

type instance L TNmAdjoinQuadDiags = TNmArc
type instance R TNmAdjoinQuadDiags = TNmQuad

instance SuperSumTy TNmTriOrQuadHalf where
    left' = mapT left'
    right' = mapT right'

instance SubSumTy TNmTriOrQuadHalf where
    either' kl kr x = either' (kl . pMap tr) (kr . pMap tr) (unT x) 
        where tr = getTriangulation x

instance SuperSumTy TNmAdjoinQuadDiags where
    left' = mapT left'
    right' = mapT right'

instance SubSumTy TNmAdjoinQuadDiags where
    either' kl kr x = either' (kl . pMap tr) (kr . pMap tr) (unT x) 
        where tr = getTriangulation x

 -- derive stuff for T-types (ignore the triangulation)
$(concatMapM (\((cls,meth),ty) -> 
    sequence [
    instanceD' (cxt[]) (cls `appT'` ty) 
        [svalD meth [| \x -> $(varE meth) x . unT |]]
         
        ]

        )

         (liftM2 (,)     
            [(''Show,'showsPrec),(''Pretty,'prettyPrec),(''ShortShow,'shortShowsPrec)]
             
            [''TNmCorner, ''TNmArc, ''TNmTri, ''TNmQuad
            , ''TNmQuadHalf, ''TNmTriOrQuadHalf, ''TNmAdjoinQuadDiags,
             ''TONmArc])
             
    )



data ConcreteNSurface = ConcreteNSurface {
    c_corners   :: [ TNmCorner   ],
    c_arcs      :: [ TNmArc      ],
    c_tris      :: [ TNmTri      ],
    c_quads     :: [ TNmQuad     ]
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
                    (Ord a, Show a, TriangulationDSnakeItem a) =>
                    (Admissible s -> [a]) -> [T a]

        f g = nub' $  map (pMap (adm_Triangulation s)) (g s) 
    


instance Vertices TNmArc where
    type Verts TNmArc = Pair (TNmCorner)
    vertices =
        traverseT map2 vertices

instance Vertices TNmAdjoinQuadDiags where
    type Verts TNmAdjoinQuadDiags = Pair (TNmCorner)
    vertices =
        traverseT map2 vertices

instance Vertices TNmTri where
    type Verts TNmTri = Triple TNmCorner
    vertices =
        traverseT map3 vertices

instance Vertices TNmQuad where
    type Verts TNmQuad = Quadruple TNmCorner
    vertices =
        traverseT map4 vertices

instance Vertices TNmQuadHalf where
    type Verts TNmQuadHalf = Triple TNmCorner
    vertices =
        traverseT map3 vertices

instance Vertices TNmTriOrQuadHalf where
    type Verts TNmTriOrQuadHalf = Triple TNmCorner
    vertices =
        traverseT map3 vertices

instance Edges TNmTri where
    type Eds TNmTri = Triple TNmArc
    edges =
        traverseT map3 edges

instance Edges TNmQuad where
    type Eds TNmQuad = Quadruple TNmArc
    edges =
        traverseT map4 edges

instance Edges TNmQuadHalf where
    type Eds TNmQuadHalf = Triple TNmAdjoinQuadDiags
    edges =
        traverseT map3 edges

instance Edges TNmTriOrQuadHalf where
    type Eds TNmTriOrQuadHalf = Triple TNmAdjoinQuadDiags
    edges =
        traverseT map3 edges





instance DeltaSet1 ConcreteNSurface

isRegardedAsSimplexByDisjointUnionDeriving ''DIM0 [t|TNmCorner|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM1 [t|TNmArc|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM1 [t|TNmAdjoinQuadDiags|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM2 [t|TNmTri|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM2 [t|TNmQuad|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM2 [t|TNmQuadHalf|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM2 [t|TNmTriOrQuadHalf|]




-- | Identity
instance TriangulationDSnakeItem NmQuadHalf where
    canonicalize_safe = const return

-- | Identity
instance TriangulationDSnakeItem NmTriOrQuadHalf where
    canonicalize_safe = const return

instance TriangulationDSnakeItem NmAdjoinQuadDiags where
    canonicalize_safe tr = 
        (fmap left' . canonicalize_safe tr) |||| 
        (fmap right' . canonicalize_safe tr)


tcnqh_quad = qh_quad . unT
tcnqh_which = qh_which . unT

instance Vertices ConcreteNSurface where
    type Verts ConcreteNSurface = [TNmCorner]
    vertices = c_corners

instance Edges ConcreteNSurface where
    type Eds ConcreteNSurface = [TNmAdjoinQuadDiags]
    edges s = (++) (map left' . c_arcs $ s) (map right' . c_quads $ s)

instance Triangles ConcreteNSurface where
    type Tris ConcreteNSurface = [TNmTriOrQuadHalf]
    triangles s = (++)
                    (map left' . c_tris $ s) 
                    [pMap (getTriangulation q) (right' (QH (w,unT q)))
                        | w <- [Half102,Half023]
                        , q <- c_quads s ]



tc_type :: (Show a, TriangulationDSnakeItem a) => T (Concrete a) -> T a
tc_type = mapT c_type

isInnerNmArc :: TNmArc -> Bool
isInnerNmArc = isInnerNormalArc . tc_type

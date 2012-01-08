{-# LANGUAGE FlexibleContexts, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric #-}
{-# OPTIONS -Wall #-}
module PreRenderable(
    module S3,
    TriangleLabel(..),
    defaultTriangleLabelUpDisplacement,
    PreRenderable(..),
    Coords(..),
    mkPreRenderable,
    tet3d,
    SimplicialTriangleLabelAssoc(..),
    sTriangleLabelAssoc,
    triangleLabelsForSimplicial,

    ) where

import Data.Vect.Double.Base
import DisjointUnion
import GHC.Generics
import S3
import ShortShow
import Simplicial.AnySimplex
import Simplicial.DeltaSet2
import Simplicial.SimplicialComplex
import Vertex
import PreRenderable.TriangleLabel
import Control.Arrow



class Coords t where
    transformCoords :: (Vec3 -> Vec3) -> t -> t

data PreRenderable s = PreRenderable {
    pr_ds :: s,
    pr_coords :: Vert s -> Vec3,
    pr_visible :: AnySimplex2 s -> Bool, 
    pr_triangleLabel :: Tri s -> Maybe TriangleLabel,
    pr_name :: AnySimplex2 s -> String
}
    deriving(Generic)

instance Coords (PreRenderable a) where
    transformCoords f _pr = _pr { pr_coords = f . pr_coords _pr }

instance 
    (   DisjointUnionable s1 s2 s
    
    ,   CoDisjointUnionable
                        (Vert s1) (Vert s2) (Vert s)
    ,   CoDisjointUnionable
                        (Tri s1) (Tri s2) (Tri s)
    ,   CoDisjointUnionable
                        (Either
                           (Either (Vert s1) (Ed s1))
                           (Tri s1))
                        (Either
                           (Either (Vert s2) (Ed s2))
                           (Tri s2))
                        (Either
                           (Either (Vert s) (Ed s)) (Tri s))
    ) =>
    DisjointUnionable (PreRenderable s1) (PreRenderable s2) (PreRenderable s) where

    disjointUnion = defaultDisjointUnion




mkPreRenderable
  :: (ShortShow (Vert s), ShortShow (Arc s), ShortShow (Tri s)) =>
     (Vert s -> Vec3) -> s -> PreRenderable s
mkPreRenderable pr_coords_ pr_ds_ = 
    PreRenderable pr_ds_ pr_coords_ (const True) (const Nothing) 
        ((shortShow|||shortShow)|||shortShow)

instance OneSkeletonable s => OneSkeletonable (PreRenderable s) where
    oneSkeleton _pr = _pr { pr_ds = oneSkeleton (pr_ds _pr) }


--type PreRenderableSimplicialComplex v = PreRenderable (OTuple v)

--tet3d :: PreRenderableSimplicialComplex Vertex
tet3d :: PreRenderable (SimplicialComplex Vertex)
tet3d = mkPreRenderable (vertexDefaultCoords . unOT) abstractTet 



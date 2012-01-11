{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric #-}
{-# OPTIONS -Wall #-}
module PreRenderable(
    module Math.Groups.S3,
    TriangleLabel(..),
    defaultTriangleLabelUpDisplacement,
    PreRenderable(..),
    Coords(..),
    mkPreRenderable,
    tet3d,
    SimplicialTriangleLabelAssoc(..),
    sTriangleLabelAssoc,
    triangleLabelsForSimplicial,
    pr_mapDs,
    pr_popDimension

    ) where

import Data.Vect.Double.Base
import DisjointUnion
import GHC.Generics
import Math.Groups.S3
import ShortShow
import Simplicial.AnySimplex
import Simplicial.DeltaSet2
import Simplicial.SimplicialComplex
import Tetrahedron.Vertex
import PreRenderable.TriangleLabel



class Coords t where
    transformCoords :: (Vec3 -> Vec3) -> t -> t

data PreRenderable s = PreRenderable {
    pr_ds :: s,
    pr_coords :: Vert s -> Vec3,
    pr_visible :: AnySimplex2Of s -> Bool, 
    pr_triangleLabel :: Tri s -> Maybe TriangleLabel,
    pr_name :: AnySimplex2Of s -> String
}
    deriving(Generic)

instance Coords (PreRenderable a) where
    transformCoords f _pr = _pr { pr_coords = f . pr_coords _pr }

instance 
    (   DisjointUnionable s1 s2 s
    
    ,   CoDisjointUnionable
                        (Vert s1) (Vert s2) (Vert s)
    ,   CoDisjointUnionable
                        (Ed s1) (Ed s2) (Ed s)
    ,   CoDisjointUnionable
                        (Tri s1) (Tri s2) (Tri s)
    ) =>
    DisjointUnionable (PreRenderable s1) (PreRenderable s2) (PreRenderable s) where

    disjointUnion = defaultDisjointUnion




mkPreRenderable
  :: (ShortShow (Vert s), ShortShow (Arc s), ShortShow (Tri s)) =>
     (Vert s -> Vec3) -> s -> PreRenderable s
mkPreRenderable pr_coords_ pr_ds_ = 
    PreRenderable pr_ds_ pr_coords_ (const True) (const Nothing) 
        (foldAnySimplex2 shortShow shortShow shortShow) 

instance OneSkeletonable s => OneSkeletonable (PreRenderable s) where
    oneSkeleton _pr = _pr { pr_ds = oneSkeleton (pr_ds _pr) }


--type PreRenderableSimplicialComplex v = PreRenderable (OTuple v)

--tet3d :: PreRenderableSimplicialComplex Vertex
tet3d :: PreRenderable (SC3 Vertex)
tet3d = mkPreRenderable vertexDefaultCoords abstractTet 


pr_mapDs
  :: (Vert t ~ Vert s,
      Ed t ~ Ed s,
      Tri t ~ Tri s) =>
     (t -> s) -> PreRenderable t -> PreRenderable s
pr_mapDs f (PreRenderable a b c d e) = PreRenderable (f a) b c d e

pr_popDimension :: PreRenderable (SC3 v) -> PreRenderable (SC2 v)
pr_popDimension = pr_mapDs sccons_skeleton

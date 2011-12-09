{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric #-}
module PreRenderable(
    module S3,
    TriangleLabel(..),
    PreRenderable(..),
    Coords(..),
    mkPreRenderable,
    PreRenderableSimplicialComplex,
    tet3d

    ) where

import Simplicial.DeltaSet
import Data.Vect.Double.Base
import S3
import GHC.Generics
import DisjointUnion
import Simplicial.AnySimplex
import Control.Exception
import Simplicial.SimplicialComplex
import Vertex


data TriangleLabel = TriangleLabel {
        tl_text :: String,

        -- | This specifies how the *vertices* of the triangle should be permuted before
        -- drawing the label with vertex 0 on the lower left, vertex 1 on the lower right,
        -- and vertex 2 on top.
        tl_transform :: S3,

        -- | 
        -- 0: Text sits on the base edge
        --
        -- 1: Text is on the same height as the top vertex
        tl_up :: Double
}
    deriving(Show,Eq)

class Coords t where
    transformCoords :: (Vec3 -> Vec3) -> t -> t

data PreRenderable a = PreRenderable {
    pr_ds :: DeltaSet a,
    pr_coords :: Vert a -> Vec3,
    pr_visible :: AnySimplex a -> Bool, 
    pr_triangleLabel :: Tri a -> Maybe TriangleLabel,
    pr_name :: AnySimplex a -> String
}
    deriving(Generic)

instance Coords (PreRenderable a) where
    transformCoords f pr = pr { pr_coords = f . pr_coords pr }

instance DisjointUnionable (PreRenderable a) (PreRenderable b) (PreRenderable (Either1 a b)) where
    disjointUnion = defaultDisjointUnion




mkPreRenderable
  :: (Show (Vert a), Show (Arc a), Show (Tri a)) =>
     (Vert a -> Vec3) -> DeltaSet a -> PreRenderable a
mkPreRenderable pr_coords pr_ds = 
    PreRenderable pr_ds pr_coords (const True) (const Nothing) 
        (\asi -> elimAnySimplexWithNat asi (\n -> 
            caseNat3 n
                show
                show
                show
                (const (assert False undefined))))

instance OrdN a => OneSkeletonable (PreRenderable a) where
    oneSkeleton pr = pr { pr_ds = oneSkeleton (pr_ds pr) }


type PreRenderableSimplicialComplex v = PreRenderable (OTuple v)

tet3d :: PreRenderableSimplicialComplex Vertex
tet3d = mkPreRenderable (vertexDefaultCoords . unOT) abstractTet 

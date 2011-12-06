{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric #-}
module PreRenderable(
    module S3,
    TriangleLabel(..),
    PreRenderable(..),
    Coords(..),
    mkPreRenderable

    ) where

import Simplicial.DeltaSet
import Data.Vect.Double.Base
import S3
import GHC.Generics
import DisjointUnion


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
    pr_visible1 :: Arc a -> Bool, 
    pr_triangleLabel :: Tri a -> Maybe TriangleLabel,
    pr_name0 :: Vert a -> String,
    pr_name1 :: Arc a -> String,
    pr_name2 :: Tri a -> String
}
    deriving(Generic)

instance Coords (PreRenderable a) where
    transformCoords f pr = pr { pr_coords = f . pr_coords pr }

instance DisjointUnionable (PreRenderable a) (PreRenderable b) (PreRenderable (Either1 a b)) where
    disjointUnion = defaultDisjointUnion




mkPreRenderable
  :: (Show (Vert a), Show (Arc a), Show (Tri a)) =>
     (Vert a -> Vec3) -> DeltaSet a -> PreRenderable a
mkPreRenderable pr_coords pr_ds = PreRenderable pr_ds pr_coords (const True) (const Nothing) show show show 



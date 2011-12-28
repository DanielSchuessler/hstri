{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, DeriveGeneric #-}
{-# OPTIONS -Wall #-}
module PreRenderable(
    module S3,
    TriangleLabel(..),
    defaultTriangleLabelUpDisplacement,
    PreRenderable(..),
    Coords(..),
    mkPreRenderable,
    PreRenderableSimplicialComplex,
    tet3d,
    SimplicialTriangleLabelAssoc(..),
    triangleLabelsForSimplicial,

    ) where

import Control.Exception
import Data.Vect.Double.Base
import DisjointUnion
import GHC.Generics
import S3
import ShortShow
import Simplicial.AnySimplex
import Simplicial.DeltaSet
import Simplicial.SimplicialComplex
import Vertex
import qualified Data.Map as M
import HomogenousTuples


data TriangleLabel = TriangleLabel {
        tl_text :: String,

        -- | This permutation maps 
        --
        -- * 0 to the index of the vertex which should be the left side of the label,
        --
        -- * 1 to the index of the vertex which should be the right side of the label,
        --
        -- * 2 to the index of the vertex which should be the top side of the label
        tl_transform :: S3,

        -- | 
        -- 0: Text sits on the base edge
        --
        -- 1: Text is on the same height as the top vertex
        tl_up :: Double
}
    deriving(Show,Eq)
         

defaultTriangleLabelUpDisplacement :: Double
defaultTriangleLabelUpDisplacement = 0.12


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
  :: (ShortShow (Vert a), ShortShow (Arc a), ShortShow (Tri a)) =>
     (Vert a -> Vec3) -> DeltaSet a -> PreRenderable a
mkPreRenderable pr_coords_ pr_ds_ = 
    PreRenderable pr_ds_ pr_coords_ (const True) (const Nothing) 
        (\asi -> elimAnySimplexWithNat asi (\n -> 
            caseNat3 n
                shortShow
                shortShow
                shortShow
                (const (assert False undefined))))

instance OrdN a => OneSkeletonable (PreRenderable a) where
    oneSkeleton pr = pr { pr_ds = oneSkeleton (pr_ds pr) }


type PreRenderableSimplicialComplex v = PreRenderable (OTuple v)

tet3d :: PreRenderableSimplicialComplex Vertex
tet3d = mkPreRenderable (vertexDefaultCoords . unOT) abstractTet 


-- | For simplicial complexes, we can simply specify a triangle -> triangle label association by the left, right and up vertex
data SimplicialTriangleLabelAssoc v = SimplicialTriangleLabelAssoc {
    stla_text :: String,
    stla_left :: v,
    stla_right :: v,
    stla_top :: v,
    stla_up :: Double
}
    deriving Show

triangleLabelsForSimplicial :: Ord v => 
    [SimplicialTriangleLabelAssoc v] -> (Tri (OTuple v)) -> Maybe TriangleLabel

triangleLabelsForSimplicial assocs = flip M.lookup (M.fromList (fmap stla2tla assocs))


stla2tla :: (Ord v) =>
    SimplicialTriangleLabelAssoc v -> (Tri (OTuple v), TriangleLabel)
stla2tla a =
            let
                lrt = map3 ($ a) (stla_left,stla_right,stla_top)
                (sorted,g) = sort3WithPermutation lrt 
            in
                (
                    assert (isOrdered3 sorted) $ OT sorted
                ,   TriangleLabel {
                        tl_text = stla_text a,
                        tl_up = stla_up a,
                        tl_transform = g
                    }
                )

    


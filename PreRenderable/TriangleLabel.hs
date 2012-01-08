{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module PreRenderable.TriangleLabel where
import S3
import PrettyUtil
import THUtil
import Simplicial.AnySimplex
import Simplicial.SimplicialComplex
import qualified Data.Map as M
import Control.Exception
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
        tl_up :: Double,

        tl_scale :: Double,

        -- | Translate label by an additional multiple of the vector from the left vertex to the right vertex (if this is 0, the center of the text is centered between the left and the right vertex) 
        tl_rightdispl :: Double

}
    deriving(Show,Eq)
         

defaultTriangleLabelUpDisplacement :: Double
defaultTriangleLabelUpDisplacement = 0.12

-- | For simplicial complexes, we can simply specify a triangle -> triangle label association by the left, right and up vertex
data SimplicialTriangleLabelAssoc v = SimplicialTriangleLabelAssoc {
    stla_text :: String,
    stla_left :: v,
    stla_right :: v,
    stla_top :: v,
    stla_up :: Double,
    stla_scale :: Double,
    stla_rightdispl :: Double
}
    deriving Show

sTriangleLabelAssoc
  :: (Eq v, Pretty v) =>
     String -> v -> v -> v -> SimplicialTriangleLabelAssoc v
sTriangleLabelAssoc txt l r t = 
    $(assrt [| l /= r && l /= t && r /= t |] ['txt,'l,'r,'t]) $ 

    SimplicialTriangleLabelAssoc txt l r t defaultTriangleLabelUpDisplacement 1 0

triangleLabelsForSimplicial :: Ord v => 
    [SimplicialTriangleLabelAssoc v] -> (OTuple v N2) -> Maybe TriangleLabel

triangleLabelsForSimplicial assocs = flip M.lookup (M.fromList (fmap stla2tla assocs))


stla2tla :: (Ord v) =>
    SimplicialTriangleLabelAssoc v -> (OTuple v N2, TriangleLabel)
stla2tla a =
            let
                lrt = map3 ($ a) (stla_left,stla_right,stla_top)
                (sorted,g) = sort3WithPermutation lrt 
            in
                (
                    assert (isOrdered3 sorted) $ OT sorted
                ,   TriangleLabel {
                        tl_transform = g,
                        
                        tl_text = stla_text a,
                        tl_up = stla_up a,
                        tl_rightdispl = stla_rightdispl a,
                        tl_scale = stla_scale a

                    }
                )

    



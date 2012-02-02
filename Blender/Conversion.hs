{-# LANGUAGE ScopedTypeVariables, RecordWildCards, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module Blender.Conversion(
    module ConcreteNormal.PreRenderable,

    -- * Convenience conversions
    fromSpqwc,
    fromSpqwcAndIntegerNormalSurface,
    fromNormalSurface,
    fromIntegerNormalSurface,

    -- * Styles
    Style(..),
    mkBlenderable,
    pseudomanifoldStyle,
    normalSurfaceStyle,
    normalSurfaceStyleB,

    -- * Class
    ToBlenderable(..),
    defaultScene,


    ) where

import Blender.Blenderable
import ConcreteNormal.PreRenderable
import Data.Function
import Data.List
import NormalEverything
import Prelude hiding(catch,mapM_,sequence_) 
import PrettyUtil
import ShortShow
import StandardCoordinates.MatchingEquations
import Data.Lens.Common
import Control.Category((>>>))

data Style = Style {
    mat0, mat1, mat2 :: Material,
    vertexThickness :: Double,
    -- | Blender groups to which all objects having this style should be added
    style_groups :: [BlenderGroupName]
}

mkBlenderable
  :: forall s. 
        Style
     -> PreRenderable s
     -> Blenderable s
mkBlenderable Style{..} pr_ = Blenderable { 
    ba_pr = pr_,

    ba_faceInfo = \asi ->
        BaFaceInfo {
            faceMat = foldAnySimplex2 (const mat0) (const mat1) (const mat2) asi,
            bfi_groups = style_groups 
        },

    ba_vertexThickness = const vertexThickness,
    ba_edgeThickness = const (edgeThicknessFactor*vertexThickness)

}




pseudomanifoldStyle :: Style
pseudomanifoldStyle = Style 
    (Material "pmMat0" (let r=0.3 in diffuseColor (r, r, r):[]) Nothing)
    (Material "pmMat1" (diffuseColor (0.7, 0.7, 0.8):specular 100 0.8) Nothing)
    (Material "pmMat2" (diffuseColor (0.7, 0.7, 0.8):specular 100 0.8) 
        (Just (Trans 0.25 0.3 1)))
    pmVertThickness  
    ["ThreeMF"]


pmVertThickness ::  Double
pmVertThickness = 0.04
edgeThicknessFactor ::  Double
edgeThicknessFactor = 0.4
nsurfVertThickness ::  Double
nsurfVertThickness = 0.03


mkNormalSurfaceStyle
  :: [Char]
     -> Triple Double -> Triple Double -> Triple Double -> Style
mkNormalSurfaceStyle suf col0 col1 col2 = Style 
    (Material ("nsurfMat0"++suf) (diffuseColor col0 :[]) Nothing)
    (Material ("nsurfMat1"++suf) (diffuseColor col1:specular 100 0.8) Nothing)
    (Material ("nsurfMat2"++suf) (diffuseColor col2:specular 100 0.8)
        (Just (Trans 0.55 0.7 1)))
    nsurfVertThickness
    ["Normal"]

normalSurfaceStyle :: Style
normalSurfaceStyle = mkNormalSurfaceStyle ""
 (0, 0.4, 1)
 (0, 0.2, 1)
 (0, 0, 1)

normalSurfaceStyleB :: Style
normalSurfaceStyleB = mkNormalSurfaceStyle "B"
 (0, 1,0)
 (0, 0.8,0)
 (0, 0.6,0)



class ToBlenderable x s | x -> s where
    toBlenderable :: x -> Blenderable s

instance ToBlenderable (Blenderable s) s where
    toBlenderable = id

-- | = 'fromSpqwc'
instance (Ord v, ShortShow v, Pretty v, Show v) => ToBlenderable (SPQWithCoords v) (SC2 v) where
    toBlenderable = fromSpqwc


fromSpqwc
  :: (Ord v, ShortShow v,Pretty v,Show v) =>
     SPQWithCoords v
     -> Blenderable (SC2 v)
fromSpqwc = mkBlenderable pseudomanifoldStyle . pr_popDimension . toPreRenderable


fromNormalSurface
  :: (i ~ Integer, Integral i, Ord v, Pretty i, ShortShow v, StandardCoords s i, Show v) =>
     SPQWithCoords v -> Admissible s -> Blenderable (SC2 (Corn v))
fromNormalSurface spqwc stc = mkBlenderable normalSurfaceStyle (normalSurfaceToPreRenderable spqwc stc) 

fromIntegerNormalSurface
  :: (Ord v, ShortShow v, StandardCoords s Integer, Show v) =>
     SPQWithCoords v -> Admissible s -> Blenderable (SC2 (Corn v))
fromIntegerNormalSurface = fromNormalSurface


fromSpqwcAndIntegerNormalSurface
  :: (Ord v,
      Show v,
      Pretty v,
      ShortShow v,
      StandardCoords s Integer) =>
     SPQWithCoords v
     -> Admissible s
     -> Blenderable
          (DJSCons
             (Asc3 v)
             (Asc3 (Corn v))
             (DJSCons (Asc2 v) (Asc2 (Corn v)) (DJSCons v (Corn v) SCMinus1)))
fromSpqwcAndIntegerNormalSurface spqwc s = 
    makeTrisAlmostInvisible
        (fromSpqwc spqwc) 
    `disjointUnion`
    fromIntegerNormalSurface spqwc s 


makeTrisAlmostInvisible :: Blenderable s -> Blenderable s
makeTrisAlmostInvisible =
    modL ba_triangleInfoL 
        (fmap 
            (setL 
                (faceMatL >>> ma_transparencyL) 
                (Just (Trans 0.05 0.05 1))))



defaultScene :: ToBlenderable a s => a -> Scene s
defaultScene = defaultScene0 . toBlenderable

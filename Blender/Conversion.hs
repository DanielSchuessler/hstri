{-# LANGUAGE ScopedTypeVariables, RecordWildCards, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-unused-binds #-}
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
import Blender.Types
import ConcreteNormal.PreRenderable
import Control.Category((>>>))
import Data.Function
import Data.Lens.Common
import Data.List
import NormalEverything
import Prelude hiding(catch,mapM_,sequence_) 
import PrettyUtil
import ShortShow
import StandardCoordinates.MatchingEquations

data Style = Style {
    mat0, mat1, mat2, mat2curved :: Material,
    style_vertexThickness :: BlenderUnits,
    -- | Blender groups to which all objects having this style should be added
    style_groups :: [BlenderGroup],
    style_helpLineMat :: Material,
    style_labelMat :: Material
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
            faceMat = 
                foldAnySimplex2 
                    (const mat0) 
                    (const mat1) 
                    (\t -> case getL pr_generalTriangleEmbeddingL pr_ t of
                                Nothing -> mat2
                                _ -> mat2curved) 
                    asi,
            bfi_groups = style_groups,
            bfi_labelMat = Just style_labelMat,
            bfi_helpLineMat = Just style_helpLineMat
        },

    ba_vertexThickness = const style_vertexThickness,
    ba_edgeThickness = const (edgeThicknessFactor*style_vertexThickness)

}



triTriTexture :: Texture
triTriTexture = ImgTex {
    tex_name = "TriTri20",
    imgtex_img = BImg {
        bimg_name = "TriTri20",
        bimg_filepath = "/h/dev/hstri/textures/TriTri20.png"
    }
}

triTriTextureSlot :: MaterialTextureSlot
triTriTextureSlot = MTS {
    mts_uv_layer = triangleUVLayerName,
    mts_tex = triTriTexture,
    mts_props = []
}


threeMFGroup :: BlenderGroup
threeMFGroup = BlenderGroup "ThreeMF"

surfaceSpecularHardness :: Int
surfaceSpecularHardness = 200

pseudomanifoldStyle :: Style
pseudomanifoldStyle = Style {
    mat0 = basicMaterial "pmMat0" (let r=0.3 in (r, r, r)),
    mat1 = mat1,
    mat2 = mat2,
    mat2curved = mat2,
    --(mat2 { ma_name = "pmMat2curved",  ma_textureSlots = [ triTriTextureSlot ] }) 
    style_vertexThickness = pmVertThickness,
    style_groups = [threeMFGroup],
    style_helpLineMat = mat1 { ma_name = "pmHelpLine" },
    style_labelMat = triLabelMat
 }


  where
    mat1 = (basicMaterial "pmMat1" (0.7, 0.7, 0.8)) {
                ma_specular_hardness = 100,
                ma_specular_intensity = 0.8
            }

    mat2 = (basicMaterial  "pmMat2" (0.7, 0.7, 0.8)) {
               ma_specular_hardness = surfaceSpecularHardness ,
               ma_specular_intensity = 0.8, 
               ma_transparency = Just (Trans 0.25 0.3 1)

            }

                
                

    triLabelMat ::  Material
    triLabelMat = (basicMaterial "triLbl" (let r = 0.015 in (r,r,r))) {
            ma_specular_hardness = 200,
            ma_specular_intensity = 0.5

        }


pmVertThickness ::  BlenderUnits
pmVertThickness = 0.04

-- | Edge thickness as a fraction of vertex sphere radius
edgeThicknessFactor ::  Double
edgeThicknessFactor = 0.4

nsurfVertThickness ::  BlenderUnits
nsurfVertThickness = 0.03

normalGroup :: BlenderGroup
normalGroup = BlenderGroup "Normal"

mkNormalSurfaceStyle
  :: [Char]
     -> Triple Double -> Triple Double -> Triple Double -> Style
mkNormalSurfaceStyle suf col0 col1 col2 = Style {
    mat0 = mat0,
    mat1 = mat1,
    mat2 = mat2,
    mat2curved = mat2,
    style_vertexThickness = nsurfVertThickness,
    style_groups = [normalGroup],
    style_helpLineMat = mat1 { ma_name = "nsurfHelpLine" },
    style_labelMat = mat0
  }

  where
    mat0 = basicMaterial ("nsurfMat0"++suf) col0
    mat1 = (basicMaterial ("nsurfMat1"++suf) col1) {
                ma_specular_hardness = 100,
                ma_specular_intensity = 0.8
            }
    mat2 =
                (basicMaterial ("nsurfMat2"++suf) col2) {
                    ma_transparency = Just (Trans 0.55 0.7 1),
                    ma_specular_hardness = surfaceSpecularHardness, 
                    ma_specular_intensity = 0.8
                }

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
          (DJSC2
             v (Corn v)
             (Asc2 v) (Asc2 (Corn v))
             (Asc3 v) (Asc3 (Corn v))
             )

fromSpqwcAndIntegerNormalSurface spqwc s = 
    makeTrisInvisible
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

makeTrisInvisible :: Blenderable s -> Blenderable s
makeTrisInvisible = modL ba_prL (pr_setTriVisibility (const OnlyLabels))  


defaultScene :: ToBlenderable a s => a -> Scene s
defaultScene = defaultScene0 . toBlenderable





{-# LANGUAGE ScopedTypeVariables, RecordWildCards, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-unused-binds #-}
module Blender.Conversion(

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
    ( Scene,
      HelpLineSettings(helpLineMat),
      Blenderable(..),
      BaFaceInfo(BaFaceInfo, bfi_groups, bfi_helpLineSettings,
                 bfi_labelMat, faceMat),
      triangleUVLayerName,
      faceMatL,
      defaultScene0,
      defaultHLS,
      ba_triangleInfoL,
      ba_prL )
import Blender.Types
    ( TransparencySettings(_fresnel, _fresnel_factor),
      Texture(..),
      MaterialTextureSlot(..),
      Material(ma_name, ma_specular_hardness, ma_specular_intensity,
               ma_transparency),
      BlenderUnits,
      BlenderImage(BImg, bimg_filepath, bimg_name),
      BlenderGroup(BlenderGroup),
      ma_transparencyL,
      defaultTrans,
      basicMaterial )
import PreRenderable
    ( Visibility(OnlyLabels),
      PreRenderable,
      pr_setTriVisibility,
      pr_popDimension,
      pr_generalTriangleImmersionL )
import SimplicialPartialQuotient ( SPQWithCoords, toPreRenderable )
import Simplicial.SimplicialComplex
    ( Triple, foldAnySimplex2, disjointUnion, Asc3, Asc2, SC2, DJSC2 )
import ConcreteNormal.PreRenderable
    ( Corn, normalSurfaceToPreRenderable )
import Control.Category ( (>>>) )
import Data.Function ( id, const, (.) )
import Data.Lens.Common ( setL, modL, getL )
import Data.List ( (++) )
import NormalEverything ( StandardCoords )
import Prelude
    ( Integral,
      Functor(fmap),
      Num((*)),
      Ord,
      Show,
      Char,
      Double,
      Int,
      Integer,
      Maybe(Just, Nothing) )

import PrettyUtil ( Pretty )
import ShortShow ( ShortShow )
import StandardCoordinates.MatchingEquations ( Admissible )



-- import Blender.Blenderable
-- import Blender.Types
-- import PreRenderable
-- import SimplicialPartialQuotient
-- import Simplicial.SimplicialComplex
-- import ConcreteNormal.PreRenderable(Corn,normalSurfaceToPreRenderable)
-- import Control.Category((>>>))
-- import Data.Function
-- import Data.Lens.Common
-- import Data.List
-- import NormalEverything
-- import Prelude hiding(catch,mapM_,sequence_) 
-- import PrettyUtil
-- import ShortShow
-- import StandardCoordinates.MatchingEquations

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
                    (\t -> case getL pr_generalTriangleImmersionL pr_ t of
                                Nothing -> mat2
                                _ -> mat2curved) 
                    asi,
            bfi_groups = style_groups,
            bfi_labelMat = Just style_labelMat,
            bfi_helpLineSettings = Just (defaultHLS { helpLineMat = Just style_helpLineMat })
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
surfaceSpecularHardness = 400

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
               ma_transparency = Just 
                    (defaultTrans 0.6) {
                        _fresnel = 2.5,
                        _fresnel_factor = 1.25
                    }

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
                    ma_transparency = Just 
                        (defaultTrans 1) {
                            _fresnel = 2.5,
                            _fresnel_factor = 1.25
                        },
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
                (Just (defaultTrans 0.05))))

makeTrisInvisible :: Blenderable s -> Blenderable s
makeTrisInvisible = modL ba_prL (pr_setTriVisibility (const OnlyLabels))  


defaultScene :: ToBlenderable a s => a -> Scene s
defaultScene = defaultScene0 . toBlenderable





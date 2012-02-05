{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Triangulation.GlueMap(
    -- * Reexports
    module EitherC,
    ErrorCall(..),

    GlueMap,gm_numberOfTetrahedra,gm_map,
    -- * Unsafe lenses (not checking invariant)
    gm_numberOfTetrahedraUnsafeL, gm_mapUnsafeL,
    -- * Construction
    unsafeMkGlueMap,mkGlueMap,mkGlueMap',
    -- * Misc
    gm_addTet
    
    
    ) 
    where

import Control.Applicative
import Control.DeepSeq.TH
import Control.Exception
import Control.Monad
import Data.Lens.Common
import Data.Lens.Template
import Data.Map(Map)
import Data.Map(Map)
import Data.Maybe
import Data.Word
import EitherC
import OrderableFace
import THUtil
import Tetrahedron
import Triangulation.FacetGluing
import Util
import qualified Data.Map as M
import Data.List(foldl')

data GlueMap = 

    -- | \"Unsafe\" due to invariants.
    UnsafeGM { 
        gm_numberOfTetrahedra :: Word,

        -- | INVARIANT: 
        --
        -- @lookup (i ./ t) gm_map = Just (j ./ packOrderedFace s g)@ 
        --
        -- implies 
        --
        -- @lookup (j ./ s) gm_map = Just (i ./ 'packOrderedFace' t ('inv' g'))@
        --
        -- (for all @f0@, @f1@, @g@).
        --
        -- INVARIANT: The 'getTIndex' of all triangles is less than 'gm_numberOfTetrahedra'.
        gm_map :: Map ITriangle OITriangle
    }
    deriving (Show,Eq,Ord)

deriveNFData ''GlueMap
nameMakeLens ''GlueMap (Just . (++"UnsafeL"))

-- | Doesn't check the invariant.
unsafeMkGlueMap = UnsafeGM

-- | Throwy variant of 'mkGlueMap''
mkGlueMap :: Word -> Map ITriangle OITriangle -> GlueMap
mkGlueMap = (.) $(unEitherC) . mkGlueMap'

mkGlueMap' :: Word -> Map ITriangle OITriangle -> EitherC (Located ErrorCall) GlueMap
mkGlueMap' n mp = do

    forM_ (M.keys mp) (\t -> unless (getTIndex t < fi n) 
                            ($failureStr ("TIndex of triangle too large: "++show t)))


    forM_ (M.keys mp) (\tri ->
        case M.lookup tri mp of
             Nothing -> return ()
             Just otri ->
                let
                    mkErrMsg msg = show tri ++ " maps to " ++ show otri ++ " but " ++ 
                                        show (forgetVertexOrder otri) ++ " " ++ msg
                in
                    case M.lookup (forgetVertexOrder otri) mp of
                        Nothing -> $(failureStr) (mkErrMsg "is not in the map.")
                        Just tri2_actual ->
                            let
                                tri2_expected =
                                    getTIndex tri                  
                                    ./                             
                                    packOrderedFace                
                                        (forgetTIndex tri)         
                                        (inv $ getVertexOrder otri)
                            in
                                unless (tri2_actual == tri2_expected) 

                                   ($(failureStr) 
                                    (mkErrMsg 
                                        ("maps to "++show tri2_actual ++ " (expected: "++
                                            show tri2_expected ++").")))

        )
            


    return (UnsafeGM n mp)

    
instance MapTIndices GlueMap where
    mapTIndices f = modL gm_mapUnsafeL ($(fromListNoCollision) . map (mapTIndices f) . M.toList) 
    
    mapTIndicesStrictlyMonotonic f = modL gm_mapUnsafeL 
        (M.fromDistinctAscList . map (mapTIndicesStrictlyMonotonic f) . M.toList) 


-- | The 'TIndex' argument to the first argument is the newly created tet
gm_addTet
  :: (TIndex -> [Gluing]) -> GlueMap -> EitherC (Located ErrorCall) (GlueMap,TIndex)
gm_addTet gls gm = (,newTet) <$> gm'
    where
        gm' =
            mkGlueMap' 
                (gm_numberOfTetrahedra gm + 1)
                (foldl' 
                    (\m g -> $insertNoCollision (glDom g) (glCod g) m) 
                    (gm_map gm) 
                    (symmetrizeGluings (gls newTet)))


        newTet = tindex . gm_numberOfTetrahedra $ gm
            

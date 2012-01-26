{-# LANGUAGE TupleSections, ViewPatterns, RecordWildCards, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module QuadCoordinates where

import Tetrahedron
import Control.Arrow((&&&))
import Control.Monad.State
import Data.Foldable(Foldable)
import Data.Function
import Data.Map as M hiding(mapMaybe)
import Data.Maybe as May
import INormalDisc
import StandardCoordinates.Class
import Test.QuickCheck
import TriangulationCxtObject
import Math.SparseVector
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.List as L
import QuadCoordinates.MatchingEquations

type QuadCoordinates r = SparseVector INormalQuad r


quad_toMap :: QuadCoordinates r -> Map INormalQuad r
quad_toMap = illdefinedSparseToMap 

quad_fromMap :: Num r => Map INormalQuad r -> QuadCoordinates r
quad_fromMap = sparse_fromMap 

quad_fromAssocs
  :: (Functor f, Num r, Foldable f) =>
     f (INormalQuad, r) -> QuadCoordinates r
quad_fromAssocs = sparse_fromAssocs


quad_coefficient :: Num r => QuadCoordinates r -> INormalQuad -> r
quad_coefficient = sparse_get 


standardToQuad :: StandardCoords a r => a -> QuadCoordinates r
standardToQuad = 
      quad_fromAssocs
    . May.mapMaybe (\(d,r) -> eitherIND (const Nothing) (\q -> Just (q,r)) d) 
    . discAssocs





            


                

                        
        
        
        
qMatchingEquation
  :: Num r => TEdge -> Maybe (QuadCoordinates r)
qMatchingEquation = fmap (quad_fromAssocs . 
    concatMap (\(_,(u,d)) -> [(u,1),(d,-1)])) . qMatchingEquation0


        
type QuadCoordinateFunctional r = QuadCoordinates r

qMatchingEquations :: Num r => Triangulation -> [QuadCoordinateFunctional r]
qMatchingEquations = mapMaybe qMatchingEquation . edges

qMatchingEquationsInteger :: Triangulation -> [QuadCoordinateFunctional Integer]
qMatchingEquationsInteger = qMatchingEquations

qMatchingEquationsRat :: Triangulation -> [QuadCoordinateFunctional Rational]
qMatchingEquationsRat = qMatchingEquations


quad_toAssocs :: QuadCoordinates r -> [(INormalQuad, r)]
quad_toAssocs = M.assocs . quad_toMap

quad_singleton :: Num r => INormalQuad -> r -> QuadCoordinates r
quad_singleton = sparse_singleton





quad_toDenseAssocs
  :: Num r =>
     Triangulation -> QuadCoordinates r -> [(INormalQuad, r)]
quad_toDenseAssocs tr qc = fmap (id &&& quad_coefficient qc) (tINormalQuads tr)

quad_toDenseList
  :: Num r => Triangulation -> QuadCoordinates r -> [r]
quad_toDenseList tr = fmap snd . quad_toDenseAssocs tr

quad_fromDenseList
  :: Num r => Triangulation -> [r] -> QuadCoordinates r
quad_fromDenseList tr = quad_fromAssocs . zip (tINormalQuads tr) 

quad_fromVector
  :: (Num r, VG.Vector v r) =>
     Triangulation -> v r -> QuadCoordinates r
quad_fromVector tr = quad_fromDenseList tr . VG.toList

unrestrictedQCGen
  :: (Num r, Arbitrary r) => Triangulation -> Gen (QuadCoordinates r)
unrestrictedQCGen tr = sparse_gen (tINormalQuads tr) 


qMatchingEquationsMatrix
  :: Num a =>
     Triangulation
     -> V.Vector (V.Vector a)
qMatchingEquationsMatrix tr = 
      V.fromList 
    . (fmap (V.fromList . quad_toDenseList tr)) 
    . qMatchingEquations  
    $ tr

qMatchingEquationsMatrixRat
  :: Triangulation
     -> V.Vector (V.Vector Rational)
qMatchingEquationsMatrixRat = qMatchingEquationsMatrix

-- test :: Triangulation -> IO ()
-- test tr = do
--     print tr
--     let testm = qMatchingEquationsMatrixRat tr
--     putStrLn . prettyMatrix $ testm
--     let (makeIntegral -> mtx',erts) = toReducedEchelon testm
--     $(assrt [| isEchelon mtx' |] ['mtx','erts]) (putStrLn "~>")
--     --mapM_ print . snd $ r
--     putStrLn . prettyMatrix $ mtx'
--     putStrLn ""        

quad_toNonzeroAssocs
  :: Num b => QuadCoordinates b -> [(INormalQuad, b)]
quad_toNonzeroAssocs = sparse_toNonzeroAssocs 




quad_fromNormalSurface
  :: (Num r, StandardCoords s r) => s -> QuadCoordinates r
quad_fromNormalSurface = quad_fromAssocs . mapMaybe f . discAssocs 
    where
        f (d,r) = eitherIND (const Nothing) (Just . (,r)) d

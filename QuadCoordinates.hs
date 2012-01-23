{-# LANGUAGE TupleSections, ViewPatterns, RecordWildCards, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module QuadCoordinates where

import AbstractNeighborhood
import AbstractTetrahedron
import Control.Applicative
import Control.Arrow((&&&))
import Control.Exception
import Control.Monad.State
import Data.EdgeLabelledTree
import Data.Foldable(Foldable)
import Data.Function
import Data.Map as M hiding(mapMaybe)
import Data.Maybe as May
import HomogenousTuples
import INormalDisc
import PrettyUtil
import QuickCheckUtil
import StandardCoordinates
import THUtil
import Test.QuickCheck
import Test.QuickCheck.All
import TriangulationCxtObject
import ZeroDefaultMap
import qualified Data.Foldable as Fold
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.SumType

newtype QuadCoordinates r = QC { quad_toZDM :: ZeroDefaultMap INormalQuad r }
    deriving(AdditiveGroup,InnerSpace,Eq)

quad_toMap :: QuadCoordinates r -> Map INormalQuad r
quad_toMap = zdm_toMap . quad_toZDM 
quad_fromMap :: Num r => Map INormalQuad r -> QuadCoordinates r
quad_fromMap = QC . zdm_fromMap 
quad_fromAssocs
  :: (Functor f, Num r, Foldable f) =>
     f (INormalQuad, r) -> QuadCoordinates r
quad_fromAssocs = QC . zdm_fromAssocs

instance Num r => VectorSpace (QuadCoordinates r) where 
    type Scalar (QuadCoordinates r) = r
    r *^ QC x = QC (r *^ x)

quad_coefficient :: Num r => QuadCoordinates r -> INormalQuad -> r
quad_coefficient = zdm_get . quad_toZDM

standardToQuad :: (Num r) => StandardCoordinates r -> QuadCoordinates r
standardToQuad = 
      quad_fromAssocs
    . May.mapMaybe (\(d,r) -> eitherIND (const Nothing) (\q -> Just (q,r)) d) 
    . stc_toAssocs

prop_standardToQuad_VertexLinkingSurface :: Triangulation -> Property
prop_standardToQuad_VertexLinkingSurface (tr :: Triangulation) = 
    forAll (elements (vertices tr)) 
        (\v -> standardToQuad (vertexLinkingSurface v) == zeroV)




            


                

                        
        
        
        
qMatchingEquation
  :: Num r => TEdge -> Maybe (QuadCoordinates r)
qMatchingEquation = fmap (quad_fromAssocs . concatMap (toList2 . snd)) . qMatchingEquation0


qMatchingEquation0
  :: (Num r) =>
     TEdge
     -> Maybe
          [(IEdgeNeighborhoodTet, ((INormalQuad, r), (INormalQuad, r)))]
qMatchingEquation0 te = do
    triPairs <- innerEdgeNeighborhood te
    (return . fmap f ) triPairs
        
  where
    f = id &&&
        (\(viewI -> I i ent ) ->
            let 
                a = ent_top ent
                b = ent_bot ent
                c = ent_left ent
                q = curry ((i./) . normalQuadByDisjointEdge . edge)
            in
                (   ( q a c, 1 ) 
                ,   ( q b c, -1 ) 
                ))

        
type QuadCoordinateFunctional = QuadCoordinates

qMatchingEquations :: Num r => Triangulation -> [QuadCoordinateFunctional r]
qMatchingEquations = mapMaybe qMatchingEquation . edges

qMatchingEquationsInteger :: Triangulation -> [QuadCoordinateFunctional Integer]
qMatchingEquationsInteger = qMatchingEquations

qMatchingEquationsRat :: Triangulation -> [QuadCoordinateFunctional Rational]
qMatchingEquationsRat = qMatchingEquations

qMatchingEquations0
  :: (Num r) =>
     Triangulation
     -> [[(IEdgeNeighborhoodTet, ((INormalQuad, r), (INormalQuad, r)))]]
qMatchingEquations0 = mapMaybe qMatchingEquation0 . edges

quad_toAssocs :: QuadCoordinates r -> [(INormalQuad, r)]
quad_toAssocs = M.assocs . quad_toMap

quad_singleton :: Num r => INormalQuad -> r -> QuadCoordinates r
quad_singleton = fmap QC . zdm_singleton


instance Pretty r => Pretty (QuadCoordinates r) where
    pretty qc = pretty (quad_toAssocs qc) 

instance (Pretty r) => Show (QuadCoordinates r) where
    showsPrec = prettyShowsPrec


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
unrestrictedQCGen tr = QC <$> zdm_gen (tINormalQuads tr) 

prop_toFromDenseList :: Triangulation -> Property
prop_toFromDenseList tr =
        forAll (unrestrictedQCGen tr) 
            (\(qc :: QuadCoordinates Int) -> 
                quad_fromDenseList tr (quad_toDenseList tr qc) .=. qc)

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
quad_toNonzeroAssocs = zdm_toNonzeroAssocs . quad_toZDM

quad_admissible
  :: (Num r, Ord r, Pretty r) => Triangulation -> QuadCoordinates r -> Either String ()
quad_admissible tr qc@(QC m) = do
    Fold.mapM_ (\r -> unless (r >= 0) (Left ("Negative coefficient"))) m
    quad_satisfiesQuadrilateralConstraints (tTetrahedra_ tr) qc 
    satisfiesQMatchingEquations tr qc

satisfiesQMatchingEquations
  :: (Pretty r, Num r) =>
     Triangulation -> QuadCoordinates r -> Either [Char] ()
satisfiesQMatchingEquations tr qc =
        mapM_ p (qMatchingEquations tr)
    where
        p me = unless (r==0)
                      (Left ("Matching equation not satisfied: "++show me++" (LHS: "++show r++")"))
            where
                r = me <.> qc

prop_satisfiesQMatchingEquationsControl
  :: Triangulation -> Property
prop_satisfiesQMatchingEquationsControl tr =
    expectFailure (forAll (unrestrictedQCGen tr
        :: Gen (QuadCoordinates Integer)) (isRight . satisfiesQMatchingEquations tr))

quad_satisfiesQuadrilateralConstraints
  :: (Num r, Ord r) =>
     [TIndex] -> QuadCoordinates r -> Either String ()
quad_satisfiesQuadrilateralConstraints tets qc = 
        mapM_ p tets
    where
        p tet = 
            unless (sum3 (map3 f (normalQuads tet)) <= (1::Int))
                   (Left ("Quadrilateral constraints violated at tet "++show tet))

        f quad = if quad_coefficient qc quad == 0 
                    then 0
                    else 1


qc_QuadCoordinates :: IO Bool
qc_QuadCoordinates = $(quickCheckAll)

quad_fromNormalSurface
  :: (Num r, NormalSurface s r) => s -> QuadCoordinates r
quad_fromNormalSurface = quad_fromAssocs . mapMaybe f . nsToAssocs 
    where
        f (d,r) = eitherIND (const Nothing) (Just . (,r)) d

{-# LANGUAGE ViewPatterns, RecordWildCards, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module QuadCoordinates where

import AbstractTetrahedron
import Control.Arrow((&&&))
import Control.Exception
import Control.Monad.State
import Data.Foldable(Foldable)
import Data.Function
import Data.Map as M hiding(mapMaybe)
import Data.Maybe as May
import HomogenousTuples
import INormalDisc
import PrettyUtil
import StandardCoordinates
import Test.QuickCheck
import Test.QuickCheck.All
import TriangulationCxtObject
import ZeroDefaultMap
import AbstractNeighbordhood

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

qc_QuadCoordinates :: IO Bool
qc_QuadCoordinates = $(quickCheckAll)


quadToStandard :: forall r. (Ord r, Num r) => 
    Triangulation -> QuadCoordinates r -> StandardCoordinates r
quadToStandard tr qc = 
        stc_fromMap (unionsWith 
                        (\ _ _ -> assert False undefined) 
                        (mapKeys iNormalDisc (quad_toMap qc)
                         :
                         vertexLinkCoeffss))
                         

    where
        vertexLinkCoeffss = fmap (mapKeys iNormalDisc . goVertex) (vertices tr)
       
        goVertex :: TVertex -> Map INormalTri r  
        goVertex v =
            let vertexLinkCoeffs = 
                    case dfsVertexLink v of
                        Node tri0 edges_ -> execState (mapM_ (goVertexLinkArc tri0) edges_) mempty
            in
                fmap 
                    (subtract (minimum (elems vertexLinkCoeffs)))
                    vertexLinkCoeffs

        
        goVertexLinkArc :: INormalTri -> 
                    (Pair INormalArc, EdgeLabelledTree INormalTri (Pair INormalArc)) ->
                    State (Map INormalTri r) ()
        goVertexLinkArc tri0 ((arc0,arc1), Node tri1 edges_) = do


            modify (\sc ->
                    insertWith
                        (\_ _ -> assert False undefined)
                        tri1
                        ( 
                            quad_coefficient qc (iNormalQuadByNormalArc arc0)
                          + (sc ! tri0)
                          - quad_coefficient qc (iNormalQuadByNormalArc arc1)
                        )
                        sc)

            mapM_ (goVertexLinkArc tri1) edges_


            


                

                        
        
        
        
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

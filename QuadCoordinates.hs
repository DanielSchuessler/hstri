{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, NoMonomorphismRestriction #-}
module QuadCoordinates where

import AbstractTetrahedron
import NormalDisc
import INormalDisc
import StandardCoordinates
import Data.Map as M
import qualified Data.List as L
import Data.Maybe as May
import MathUtil
import Data.VectorSpace
import Test.QuickCheck
import TriangulationCxtObject
import Test.QuickCheck.All
import PrettyUtil
import Control.Monad.State
import Control.Exception
import HomogenousTuples
import qualified Data.Vector as V
import qualified Data.Foldable as Fold
import QuickCheckUtil

newtype QuadCoordinates r = QC { quad_toMap :: Map INormalQuad r }
    deriving(AdditiveGroup,InnerSpace,Pretty)

quad_fromMap = QC

instance Num r => VectorSpace (QuadCoordinates r) where 
    type Scalar (QuadCoordinates r) = r
    r *^ QC x = QC (r *^ x)

quad_coefficient (QC m) q = fromMaybe 0 (M.lookup q m) 

standardToQuad :: StandardCoordinates r -> Map INormalQuad r
standardToQuad = 
      M.fromList
    . May.mapMaybe (\(d,r) -> eitherIND (const Nothing) (\q -> Just (q,r)) d) 
    . stc_toAssocs

prop_standardToQuad_VertexLinkingSurface :: Triangulation -> Property
prop_standardToQuad_VertexLinkingSurface (tr :: Triangulation) = 
    forAll (elements (vertices tr)) 
        (\v -> standardToQuad (vertexLinkingSurface v) == zeroV)

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
                        Node tri0 edges -> execState (mapM_ (goVertexLinkArc tri0) edges) mempty
            in
                fmap 
                    (subtract (minimum (elems vertexLinkCoeffs)))
                    vertexLinkCoeffs

        
        goVertexLinkArc :: INormalTri -> 
                    (Pair INormalArc, EdgeLabelledTree INormalTri (Pair INormalArc)) ->
                    State (Map INormalTri r) ()
        goVertexLinkArc tri0 ((arc0,arc1), Node tri1 edges) = do


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

            mapM_ (goVertexLinkArc tri1) edges


            


data EdgeNeighborhood = 
    ENTet TIndex (OTriangle,Maybe EdgeNeighborhood) (OTriangle,Maybe EdgeNeighborhood)
                
                



                        
abstractEdgeNeighborhood :: S2 -> TEdge -> [I (Pair OTriangle)]
abstractEdgeNeighborhood dir te = 
    let
        tr = getTriangulation te
        
        e = unT te
        i0 = getTIndex e        

        (_S0,_T0) = map2 otriangle ((v0,v1,v2), (v0,v1,v3))
            where
                (v0,v1) = vertices (forgetTIndex e)
                (v2,v3) = dir .* vertices (oppositeEdge (forgetTIndex e))
    in 
        (I i0 (_S0,_T0)) :

        L.unfoldr (\_Tprev -> do
                        _S <- lookupGluingOfOITriangle tr _Tprev

                        let I i _S' = viewI _S

                            _T = case vertices _S' of
                                   (v0,v1,v2) ->
                                       otriangle (v0,v1,otriangleDualVertex _S')
                            
                        Just (I i (_S',_T), 
                              i ./ _T))

                  (i0 ./ _T0)




    
innerEdgeNeighborhood :: TEdge -> Maybe [I (OTriangle, OTriangle)]
innerEdgeNeighborhood te = 
    let
        x0@(I i0 (_S0, _)) : xs = abstractEdgeNeighborhood NoFlip te

    in
        case break (\(I i (_S, _T)) -> (i,_S) == (i0,_S0)) xs of

             (l,_:_) -> Just (x0:l)
             (_,[]) -> Nothing

        
prop_innerEdgeNeighborhood (tr :: Triangulation) =
    forAllElements (edges tr)
        (\te -> case innerEdgeNeighborhood te of
                     Nothing -> property (isBoundaryEdge te)
                     Just xs -> length xs .=.
                                ecSize (edgePreimage te))

  where
    isBoundaryEdge = 
        any (isNothing . lookupGluingOfITriangle tr)
            . itrianglesContainingEdge
        
        
        




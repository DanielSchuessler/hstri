{-# LANGUAGE ViewPatterns, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables #-}
module QuadCoordinates.CanonExt where

import Control.Exception
import Control.Monad.State.Lazy
import Data.EdgeLabelledTree
import Data.Map(Map,unionsWith,singleton,elems,insertWith,(!))
import HomogenousTuples
import PrettyUtil
import QuadCoordinates.Class
import QuadCoordinates.MatchingEquations
import StandardCoordinates.Class
import StandardCoordinates.MatchingEquations
import THUtil
import TriangulationCxtObject
import Math.SparseVector
import qualified Data.Map as M
import Triangulation.Class
import Control.DeepSeq.TH


canonExtDbg = canonExt

--canonExtDbg tr qc =
--     case canonExt tr qc of
--          sc ->
-- 
--              case admissible tr sc of
--                 Right () -> sc
--                 Left str -> error ("canonExtDbg: result not admissible:\n"++str++"\n"++
--                                     $(showExps ['qc,'sc]))
-- 
data CanonExt q r = CanonExt {
    canonExt_quads :: q,
    canonExt_tris :: SparseVector INormalTri r
}
    deriving (Eq, Ord, Show)

instance (Pretty q, Pretty r, Num r, Ord r) => Pretty (CanonExt q r) where
    prettyPrec prec x = parensIf (prec>10)
        (prettyRecord "CanonExt"
            [("quads",pretty (canonExt_quads x))
            ,("tris",pretty (canonExt_tris x))])

instance QuadCoords q r => QuadCoords (CanonExt q r) r where
    quadCount = quadCount . canonExt_quads
    quadAssocs = quadAssocs . canonExt_quads
    quadAssocsDistinct = quadAssocs . canonExt_quads

instance (Num r, QuadCoords q r) => StandardCoords (CanonExt q r) r where
    discCount = default_discCount_from_triQuadCount
    discAssocs = default_discAssocs_from_triQuadAssocs
    discAssocsDistinct = default_discAssocsDistinct_from_triQuadAssocsDistinct

    standardAsSparse = sparse_fromAssocs . discAssocsDistinct


    triCount = sparse_get . canonExt_tris 
    triAssocs = triAssocsDistinct
    triAssocsDistinct = sparse_toAssocs . canonExt_tris 

canonExt :: forall q r tr. (Pretty q, QuadCoords q r, Ord r, Pretty r) => 
    QAdmissible q -> Admissible (CanonExt q r)
canonExt qc = 

             case admissible tr preResult of
                Right x -> x
                Left str -> error ("canonExtDbg: result not admissible:\n"++str++"\n"++
                                    $(showExps ['qc,'preResult]))


    where
        tr = qadm_Triangulation qc
     
        preResult = CanonExt (qadm_coords qc)
                        (sparse_fromMap 
                        (unionsWith 
                            (\ _ _ -> assert False undefined) 
                            vertexLinkCoeffss))


        vertexLinkCoeffss = map goVertex (vertices tr)
       
        goVertex :: TVertex -> Map INormalTri r  
        goVertex v =
            let vertexLinkCoeffs = 
                    case dfsVertexLink v of
                        Node tri0 edges_ -> 
                            execState 
                                (mapM_ (goVertexLinkArc tri0) edges_) 
                                (singleton tri0 0)
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
                        (\_ _ -> 
                            error ("canonExt: tri1 visited twice"
                                    ++ $(showExps ['qc,'tri0,'arc0,'arc1,'tri1,'edges_])))
                        tri1
                        ( 
                            quadCount qc (iNormalQuadByNormalArc arc0)
                          + (sc ! tri0)
                          - quadCount qc (iNormalQuadByNormalArc arc1)
                        )
                        sc)

            mapM_ (goVertexLinkArc tri1) edges_


deriveNFData ''CanonExt

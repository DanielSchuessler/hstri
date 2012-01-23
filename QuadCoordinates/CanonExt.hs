{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module QuadCoordinates.CanonExt where

import QuadCoordinates.Class
import StandardCoordinates
import TriangulationCxtObject
import PrettyUtil
import NormalSurface


canonExtDbg tr qc =
    case canonExt tr qc of
         sc ->

             case admissible tr sc of
                Right () -> sc
                Left str -> error ("canonExtDbg: result not admissible:\n"++str++"\n"++
                                    $(showExps ['qc,'sc]))

data CanonExt q r = CanonExt {
    canonExt_quads :: q,
    canonExt_tris :: ZeroDefaultMap INormalTri r
}

instance QuadCoords q e => QuadCoords (CanonExt q r) where
    quadCount = quadCount . canonExt_quads
    quadAssocs = quadAssocs . canonExt_quads

instance QuadCoords q e => NormalSurface (CanonExt q r) where
    q

canonExt tr qc = 
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
                            quad_coefficient qc (iNormalQuadByNormalArc arc0)
                          + (sc ! tri0)
                          - quad_coefficient qc (iNormalQuadByNormalArc arc1)
                        )
                        sc)

            mapM_ (goVertexLinkArc tri1) edges_



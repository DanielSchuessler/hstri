{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module StandardCoordinates.MatchingEquations where

import StandardCoordinates.Class
import QuadCoordinates.Class
import Control.Monad
import Triangulation
import HomogenousTuples
import Tetrahedron.NormalDisc


admissible
  :: (Ord a, StandardCoords q a) =>
     Triangulation -> q -> Either [Char] ()
admissible tr stc = do
    mapM_ (\r -> unless (snd r >= 0) (Left ("Negative coefficient")))
          (nsToAssocs stc)
    satisfiesQuadrilateralConstraints tr stc 
    satisfiesMatchingEquations tr stc

satisfiesMatchingEquations
  :: StandardCoords q a => Triangulation -> q -> Either [Char] ()
satisfiesMatchingEquations tr stc =
        mapM_ p (matchingEquationReasons tr)
    where
        p me = unless (r==0)
                      (Left ("Matching equation not satisfied: "++show me++" (LHS: "++show r++")"))
            where
                r = evalMatchingEquation me stc

satisfiesQuadrilateralConstraints
  :: QuadCoords q a => Triangulation -> q -> Either [Char] ()
satisfiesQuadrilateralConstraints tr stc = 
        mapM_ p tets
    where
        tets = tTetrahedra_ tr
        
        p tet = 
            unless (sum3 (map3 f (normalQuads tet)) <= (1::Int))
                   (Left ("Quadrilateral constraints violated at tet "++show tet))

        f quad = if quadCount stc quad == 0 
                    then 0
                    else 1




data MatchingEquationReason = MatchingEquationReason ITriangle OITriangle Vertex Vertex

instance Show MatchingEquationReason where
    show (MatchingEquationReason x x' v v')
        = unwords [ "Gluing", show x, "to", show x', 
                    "identifies the normal arcs",
                    show (iNormalArc (x, v)),
                    "and",
                    show (iNormalArc (x', v'))
                  ]
                            
matchingEquationReasons
  :: Triangulation -> [MatchingEquationReason]
matchingEquationReasons t =
      [
         MatchingEquationReason x x' (forgetTIndex v) (forgetTIndex v')
               
        |
            (x,x') <- tGluingsIrredundant t,
            (v,v') <- zip (vertexList x) (vertexList x')

            ]



evalMatchingEquation
  :: StandardCoords q a => MatchingEquationReason -> q -> a
evalMatchingEquation (MatchingEquationReason x x' v v') stc =
                              (triCount stc (iNormalTri $ getTIndex x ./ v)
                          + quadCount stc (iNormalQuadByVertexAndITriangle v x))

                          - triCount stc (iNormalTri $ getTIndex x' ./ v')
                          - quadCount stc (iNormalQuadByVertexAndITriangle v' (forgetVertexOrder x'))

    



{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
{-# OPTIONS -Wall #-}
module QuadCoordinates.MatchingEquations where
import AbstractNeighborhood
import AbstractTetrahedron
import Control.Arrow((&&&))
import Control.Monad.State
import Data.Function
import Data.Maybe as May
import INormalDisc
import StandardCoordinates.MatchingEquations
import QuadCoordinates.Class
import TriangulationCxtObject
import Data.List as L


quad_admissible
  :: (Ord r, QuadCoords q r) =>
     Triangulation -> q -> Either [Char] ()
quad_admissible tr qc = do
    mapM_ (\r -> unless (snd r >= 0) (Left ("Negative coefficient"))) (quadAssocs qc) 
    satisfiesQuadrilateralConstraints tr qc 
    satisfiesQMatchingEquations tr qc

evalQMatchingEquation
  :: QuadCoords q b => QMatchingEquation -> q -> b
evalQMatchingEquation (me :: QMatchingEquation) qc =
    foldl' (+) 0 
    . L.map (\(_,(u,d)) -> quadCount qc u - quadCount qc d) 
    $ me


satisfiesQMatchingEquations
  :: QuadCoords q r => Triangulation -> q -> Either [Char] ()
satisfiesQMatchingEquations tr qc =
        mapM_ p (qMatchingEquations0 tr)
    where
        p me = unless (r==0)
                      (Left ("Matching equation not satisfied: "++show me++" (LHS: "++show r++")"))
            where
                r = evalQMatchingEquation me qc

type QMatchingEquation = [(IEdgeNeighborhoodTet, 
    (
    INormalQuad {- upwards sloped -}, 
    INormalQuad {- downwards sloped -}
    )
    )]

qMatchingEquation0
  ::
     TEdge
     -> Maybe
          QMatchingEquation
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
                (   q a c
                ,   q b c
                ))
qMatchingEquations0
  ::
     Triangulation
     -> [QMatchingEquation]
qMatchingEquations0 = mapMaybe qMatchingEquation0 . edges


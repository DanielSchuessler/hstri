{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, ViewPatterns, TemplateHaskell, NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module StandardCoordinates.MatchingEquations where

import StandardCoordinates.Class
import QuadCoordinates.Class
import Control.Monad
import Triangulation
import HomogenousTuples
import Tetrahedron.NormalDisc
import TupleTH
import Data.SumType
import Triangulation.Class
import Data.Function
import PrettyUtil

isAdmissible
  :: (ToTriangulation tr, StandardCoords s r) => tr -> s -> Bool
isAdmissible = (.) isRight . admissible

admissible
  :: (ToTriangulation tr, StandardCoords s r) =>
     tr -> s -> Either [Char] (Admissible s)
admissible (toTriangulation -> tr) stc = do
    mapM_ (\r -> unless (snd r >= 0) (Left ("Negative coefficient")))
          (discAssocs stc)
    satisfiesQuadrilateralConstraints tr stc 
    satisfiesMatchingEquations tr stc
    return (UnsafeToAdmissible tr stc)

satisfiesMatchingEquations
  :: StandardCoords q r => Triangulation -> q -> Either [Char] ()
satisfiesMatchingEquations tr stc =
        mapM_ p (matchingEquationReasons tr)
    where
        p me = unless (r==0)
                      (Left ("Matching equation not satisfied: "++show me++" (LHS: "++show r++")"))
            where
                r = evalMatchingEquation me stc

satisfiesQuadrilateralConstraints
  :: QuadCoords s r => Triangulation -> s -> Either [Char] ()
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
  :: StandardCoords s r => MatchingEquationReason -> s -> r
evalMatchingEquation (MatchingEquationReason x x' v v') stc =
                              (triCount stc (iNormalTri $ getTIndex x ./ v)
                          + quadCount stc (iNormalQuadByVertexAndITriangle v x))

                          - triCount stc (iNormalTri $ getTIndex x' ./ v')
                          - quadCount stc (iNormalQuadByVertexAndITriangle v' (forgetVertexOrder x'))

    

matchingEquationSupport
  :: MatchingEquationReason
     -> (INormalTri, INormalQuad, INormalTri, INormalQuad)
matchingEquationSupport (MatchingEquationReason x x' v v') =
                        
                          (iNormalTri $ getTIndex x ./ v
                          ,iNormalQuadByVertexAndITriangle v x
                          ,iNormalTri $ getTIndex x' ./ v'
                          ,iNormalQuadByVertexAndITriangle v' (forgetVertexOrder x')
                          )

matchingEquationSupportDiscs
  :: MatchingEquationReason
     -> (INormalDisc, INormalDisc, INormalDisc, INormalDisc)
matchingEquationSupportDiscs =
    $(mapTuple' 4 [|iNormalDisc|]) . matchingEquationSupport


data Admissible q = UnsafeToAdmissible {
    adm_Triangulation :: Triangulation,
    adm_coords :: q 
}
    deriving(Show)


-- | Does /not/ compare the triangulations
instance Eq q => Eq (Admissible q) where
    (==) = (==) `on` adm_coords

-- | Does /not/ compare the triangulations
instance Ord q => Ord (Admissible q) where
    compare = compare `on` adm_coords

instance QuadCoords q r => QuadCoords (Admissible q) r where
    quadCount = quadCount . adm_coords
    quadAssocs = quadAssocs . adm_coords
    quadAssocsDistinct = quadAssocsDistinct . adm_coords
    quadAsSparse = quadAsSparse . adm_coords

instance StandardCoords s r => StandardCoords (Admissible s) r where
    discCount = discCount . adm_coords
    discAssocs = discAssocs . adm_coords
    discAssocsDistinct = discAssocsDistinct . adm_coords
    standardAsSparse = standardAsSparse . adm_coords

    triCount = triCount . adm_coords
    triAssocs = triAssocs . adm_coords
    triAssocsDistinct = triAssocsDistinct . adm_coords

unsafeToAdmissible :: Triangulation -> q -> Admissible q
unsafeToAdmissible = UnsafeToAdmissible

instance Pretty s => Pretty (Admissible s) where
    prettyPrec prec = prettyPrec prec . adm_coords


toAdmissible
  :: (Show a, ToTriangulation tr, StandardCoords a r) =>
     tr -> a -> Admissible a
toAdmissible tr x = either _err id . admissible tr $ x
    where
        _err e = error ("toAdmissible "++showsPrec 11 x ""++": "++e)
    

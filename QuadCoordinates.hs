{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, NoMonomorphismRestriction #-}
module QuadCoordinates where

import AbstractTetrahedron
import NormalDisc
import INormalDisc
import StandardCoordinates
import Data.Map as M
import Data.List as L
import Data.Maybe as May
import MathUtil
import Data.VectorSpace
import Test.QuickCheck
import TriangulationCxtObject
import Test.QuickCheck.All

newtype QuadCoordinates r = QC (Map INormalQuad r) 
    deriving(AdditiveGroup,InnerSpace)

instance Num r => VectorSpace (QuadCoordinates r) where 
    type Scalar (QuadCoordinates r) = r
    r *^ QC x = QC (r *^ x)

qcCoefficient (QC m) q = fromMaybe 0 (M.lookup q m) 

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

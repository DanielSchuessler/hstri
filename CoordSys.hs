{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, TypeSynonymInstances, EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module CoordSys where

import StandardCoordinates.Dense()
import Tetrahedron.INormalDisc
import Data.FormalOps
import Math.SparseVector
import Data.Proxy
import Data.BitVector.Adaptive
import Util
import NormalSurfaceBasic
import QuadCoordinates.MatchingEquations
import StandardCoordinates.MatchingEquations
import Triangulation
import CheckAdmissibility
import QuadCoordinates.Dense
import qualified Data.Vector as V
import Data.Vector(Vector)
import MathUtil
import QuadCoordinates.Class
import QuadCoordinates
import FileLocation
import qualified Data.List as L



-- | Implementations must not look at the (type proxy) 'c' arg
class ( CheckAdmissibility c (WrappedVector c Vector Rational) Rational
      , RatioToIntegral 
            (AdmissibleFor c (WrappedVector c Vector Rational))
            (AdmissibleFor c (WrappedVector c Vector Integer))
      , NonNegScalable Rational (AdmissibleFor c (WrappedVector c Vector Rational))
      ) 

        => CoordSys c where

    numberOfVariables :: Proxy c -> Triangulation -> Int
    -- |  Matching equations
    hyperplanes :: Proxy c -> Triangulation -> [[Rational]]
    -- | Only one per tetrahedron (the other two must have index 1 resp. 2 higher)
    quadIndexOffsets :: Proxy c -> Triangulation -> [BitVectorPosition]

    discIndexToDisc :: Proxy c -> BitVectorPosition -> INormalDisc 


instance CoordSys QuadCoordSys where
    numberOfVariables _ = tNumberOfNormalQuadTypes 
    hyperplanes _ tr = fmap (quad_toDenseList tr) . qMatchingEquationsRat $ tr
    quadIndexOffsets _ tr = map (3*) [0.. tNumberOfTetrahedra tr - 1]
    discIndexToDisc _ = iNormalQuadToINormalDisc . toEnum


instance CoordSys StdCoordSys where
        numberOfVariables _ = tNumberOfNormalDiscTypes
        hyperplanes _ tr = 
            [ map (toRational . evalMatchingEquation me) (tINormalDiscs tr)
                | me <- matchingEquationReasons tr ]

        quadIndexOffsets _ tr = 
                        [ 
--                             (fromEnum . iNormalQuadToINormalDisc) (tindex i ./ minBound)
                            7*i+4
                                | i <- [0.. tNumberOfTetrahedra tr - 1] ]

        discIndexToDisc _ = toEnum






-- class (Num r, Ord r) => NormalSurfaceCoords coordsys s r | coordsys s -> r
-- 
-- #define F(SYS)\
-- instance NormalSurfaceCoords SYS INormalQuad Integer where {};\
-- instance NormalSurfaceCoords SYS INormalDisc Integer where {};\
-- instance NormalSurfaceCoords SYS INormalTri Integer where {};\
-- instance (Num n, NormalSurfaceCoords SYS q n) => NormalSurfaceCoords SYS [q] n where {};\
-- instance (Num n, NormalSurfaceCoords SYS q n) => NormalSurfaceCoords SYS (FormalProduct n q) n where {};\
-- instance (Num n, NormalSurfaceCoords SYS q n, NormalSurfaceCoords SYS q' n) => NormalSurfaceCoords SYS (FormalSum q q') n where {};\
-- 
-- 
-- instance (Ord i, Num i) => NormalSurfaceCoords QuadCoordSys (SparseVector INormalQuad i) i where
-- instance (Ord i, Num i) => NormalSurfaceCoords QuadCoordSys (SparseVector INormalDisc i) i where
-- instance (Ord i, Num i) => NormalSurfaceCoords StdCoordSys (SparseVector INormalDisc i) i where
-- 
-- F(QuadCoordSys)
-- F(StdCoordSys)
--
    



newtype ZeroSet coords w = ZeroSet { unZeroSet :: w}
    deriving(Eq,Ord,Show,BitVector,FixedBitVector)


{-# INLINABLE zeroSetSatisfiesQuadConstraintsAtOffset #-}
zeroSetSatisfiesQuadConstraintsAtOffset
  :: BitVector w => w -> BitVectorPosition -> Bool
zeroSetSatisfiesQuadConstraintsAtOffset z i = 
                    atLeastTwo
                        (bvUnsafeIndex z i)
                        (bvUnsafeIndex z (i+1))
                        (bvUnsafeIndex z (i+2))
                        
                        

zeroSetAdmissible
  :: (BitVector w, CoordSys coords) =>
     Triangulation -> ZeroSet coords w -> Bool
zeroSetAdmissible tr (z :: ZeroSet coords w) =

            all (zeroSetSatisfiesQuadConstraintsAtOffset z)

                (quadIndexOffsets ($undef :: Proxy coords) tr)



findTetViolatingQuadConstraints
  :: (BitVector w, CoordSys coords) =>
     Triangulation -> ZeroSet coords w -> Maybe TIndex
findTetViolatingQuadConstraints tr (z :: ZeroSet coords w) =
            fmap (getTIndex . discIndexToDisc co) $

            L.find (not . zeroSetSatisfiesQuadConstraintsAtOffset z)

                (quadIndexOffsets co tr)


    where co = ($undef :: Proxy coords)



-- | Indices of bits not set, up to the 'numberOfVariables'
zeroSetComplElements
  :: (BitVector w, CoordSys c) =>
     Proxy c -> Triangulation -> w -> [Int]
zeroSetComplElements co tr zs = [ i | i <- [0..numberOfVariables co tr-1], not (bvUnsafeIndex zs i) ]


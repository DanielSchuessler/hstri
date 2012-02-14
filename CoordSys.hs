{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, NoMonomorphismRestriction, FlexibleContexts, FlexibleInstances, TypeSynonymInstances, EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
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



-- | Implementations must not look at the (type proxy) 'c' arg
class ( CheckAdmissibility c (WrappedVector c Vector Rational) Rational
      , RatioToIntegral 
            (AdmissibleFor c (WrappedVector c Vector Rational))
            (AdmissibleFor c (WrappedVector c Vector Integer))
      , NonNegScalable Rational (AdmissibleFor c (WrappedVector c Vector Rational))
      ) 

        => CoordSys c where

    numberOfVariables :: Proxy c -> Triangulation -> Int
    hyperplanes :: Proxy c -> Triangulation -> [[Rational]]
    quadIndexOffsets :: Proxy c -> Triangulation -> [BitVectorPosition]


instance CoordSys QuadCoordSys where
    numberOfVariables _ = tNumberOfNormalQuadTypes 
    hyperplanes _ tr = fmap (quad_toDenseList tr) . qMatchingEquationsRat $ tr
    quadIndexOffsets _ tr = map (3*) [0.. tNumberOfTetrahedra tr - 1]


instance CoordSys StdCoordSys where
        numberOfVariables _ = tNumberOfNormalDiscTypes
        hyperplanes _ tr = 
            [ map (toRational . evalMatchingEquation me) (tINormalDiscs tr)
                | me <- matchingEquationReasons tr ]

        quadIndexOffsets _ tr = 
                        [ (fromEnum . iNormalQuadToINormalDisc) (tindex i ./ minBound)
                                | i <- [0.. tNumberOfTetrahedra tr - 1] ]






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


zeroSetAdmissible
  :: (BitVector w, CoordSys coords) =>
     Triangulation -> ZeroSet coords w -> Bool
zeroSetAdmissible tr (z :: ZeroSet coords w) =

            all (\i ->
                    atLeastTwo
                        (bvUnsafeIndex z i)
                        (bvUnsafeIndex z (i+1))
                        (bvUnsafeIndex z (i+2))
                        
                        
                        )

                (quadIndexOffsets ($undef :: Proxy coords) tr)



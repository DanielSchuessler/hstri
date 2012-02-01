{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module CoordSys where
import INormalDisc
import Data.FormalOps
import Math.SparseVector
import Data.Proxy


data QuadCoordSys

quadCoordSys :: Proxy QuadCoordSys
quadCoordSys = undefined

data StdCoordSys

stdCoordSys :: Proxy StdCoordSys
stdCoordSys = undefined

class (Num r, Ord r) => NormalSurfaceCoefficients s r | s -> r

instance NormalSurfaceCoefficients INormalQuad Integer where {}
instance NormalSurfaceCoefficients INormalDisc Integer where {}
instance NormalSurfaceCoefficients INormalTri Integer where {}
instance (Num n, NormalSurfaceCoefficients q n) => NormalSurfaceCoefficients [q] n where {}
instance (Num n, NormalSurfaceCoefficients q n) => NormalSurfaceCoefficients (FormalProduct n q) n where {}
instance (Num n, NormalSurfaceCoefficients q n, NormalSurfaceCoefficients q' n) => NormalSurfaceCoefficients (FormalSum q q') n where {}

instance (Ord i, Num i) => NormalSurfaceCoefficients (SparseVector INormalQuad i) i where
instance (Ord i, Num i) => NormalSurfaceCoefficients (SparseVector INormalDisc i) i where


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

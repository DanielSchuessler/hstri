{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module NormalSurfaceBasic where
import Data.Proxy
import Data.FormalOps
import Tetrahedron.INormalDisc
import Math.SparseVector

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


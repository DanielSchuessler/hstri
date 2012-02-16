{-# LANGUAGE DeriveDataTypeable, TypeFamilies, FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module NormalSurfaceBasic where
import Data.Proxy
import Data.FormalOps
import Tetrahedron.INormalDisc
import Math.SparseVector
import Element
import Data.Typeable

data QuadCoordSys
    deriving Typeable

quadCoordSys :: Proxy QuadCoordSys
quadCoordSys = undefined

data StdCoordSys
    deriving Typeable

stdCoordSys :: Proxy StdCoordSys
stdCoordSys = undefined

-- | Wrapper type for vectors admissible with respect to the normal surface coordinate system @c@.
data family AdmissibleFor c :: (* -> *)

deriving instance Typeable2 AdmissibleFor

type instance Element (AdmissibleFor c s) = Element s

class (Num r, Ord r) => NormalSurfaceCoefficients s r | s -> r

instance NormalSurfaceCoefficients INormalQuad Integer where {}
instance NormalSurfaceCoefficients INormalDisc Integer where {}
instance NormalSurfaceCoefficients INormalTri Integer where {}
instance (Num n, NormalSurfaceCoefficients q n) => NormalSurfaceCoefficients [q] n where {}
instance (Num n, NormalSurfaceCoefficients q n) => NormalSurfaceCoefficients (FormalProduct n q) n where {}
instance (Num n, NormalSurfaceCoefficients q n, NormalSurfaceCoefficients q' n) => NormalSurfaceCoefficients (FormalSum q q') n where {}

instance (Ord i, Num i) => NormalSurfaceCoefficients (SparseVector INormalQuad i) i where
instance (Ord i, Num i) => NormalSurfaceCoefficients (SparseVector INormalDisc i) i where


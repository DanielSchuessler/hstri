{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module VerboseDD.Types where
import PrettyUtil
import Data.BitVector.Adaptive
import Control.Monad.Writer.Lazy
import Control.Monad.State.Strict
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Data.Vector(Vector)
import Data.DList
import Control.Arrow
import CoordSys
import Data.Function

newtype VectorIndex = VectorIndex Int
    deriving(Eq,Enum,Ord,Real,Num,Integral)

instance Show VectorIndex where
    show (VectorIndex i) = pad 3 . show $ i

instance Pretty VectorIndex where
    pretty = dullyellow . text . show



data PairFate v = 
    PairFate {
        pf_fst, pf_snd :: v,
        pf_kind :: PairFateKind v
    }


instance Pretty (PairFate v) => Show (PairFate v) where showsPrec = prettyShowsPrec



data PairFateKind v =     
        Incompatible
    |   NotAdjacent v
    |   OK VectorIndex

    deriving(Show)

instance Show v => Pretty (PairFateKind v) where pretty = string . show

data DDStepResult v = DDSR { 
    _Sneg, _S0, _Spos :: Vector v, 
    pairFates :: Vector (PairFate v) 
}

type DDLog v = DList (DDStepResult v)

newtype VerboseDD v a = VerboseDD (WriterT (DDLog v) (State VectorIndex) a)
    deriving(Functor,Monad,MonadWriter (DDLog v),MonadState VectorIndex)


nextIndex :: VerboseDD v VectorIndex
nextIndex = do
    i <- get
    put (succ i)
    return i


runVerboseDD :: VerboseDD v a -> (a, [DDStepResult v])
runVerboseDD (VerboseDD x) = second toList (evalState (runWriterT x) 0)


-- data VerboseDDVectorPairWithZeroSetIntersection v co w =
--     VerboseDDVectorPairWithZeroSetIntersection !v !v !(ZeroSet co w)


class (BitVector w) => VerboseDDVectorRepresentation a co w | a -> co w where

    ddrep_index :: a -> VectorIndex
    ddrep_zeroSet :: a -> ZeroSet co w 






findVectorsWithZeroSetAtLeast
  :: (VG.Vector v a, VerboseDDVectorRepresentation a co w) =>
     ZeroSet co w -> v a -> v a
findVectorsWithZeroSetAtLeast minZeroSet _Vp =
        VG.filter (\z -> 
            bvSubset minZeroSet (ddrep_zeroSet z)) 

            _Vp

findVectorDifferentThanTheseAndWithZeroSetAtLeast
  :: (VG.Vector v a, VerboseDDVectorRepresentation a co w) =>
     a -> a -> ZeroSet co w -> v a -> Maybe a
findVectorDifferentThanTheseAndWithZeroSetAtLeast x y minZeroSet _Vp  =
    VG.find (liftM2 (||) (ddrep_differentIndex x) (ddrep_differentIndex y))
                            $ findVectorsWithZeroSetAtLeast minZeroSet _Vp 

    where
        ddrep_differentIndex = (/=) `on` ddrep_index

intersectZeroSets :: VerboseDDVectorRepresentation a co w => a -> a -> ZeroSet co w
intersectZeroSets = bvIntersection `on` ddrep_zeroSet


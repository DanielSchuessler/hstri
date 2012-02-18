{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module VerboseDD.Types where
import PrettyUtil
import Data.BitVector.Adaptive
import Control.Monad.Writer.Lazy
import Control.Monad.State.Strict
import qualified Data.Vector.Generic as VG
import Data.DList
import CoordSys
import Data.Function
import Control.Arrow(second)
import Control.Applicative

newtype VectorIndex = VectorIndex Int
    deriving(Eq,Enum,Ord,Real,Num,Integral)

instance Show VectorIndex where
    show (VectorIndex i) = pad 3 . show $ i

instance Pretty VectorIndex where
    pretty = dullyellow . text . show

prettyVI :: VectorIndex -> Doc
prettyVI (VectorIndex i) = dullyellow . text . show $ i



data PairFate v = 
    PairFate {
        pf_fst, pf_snd :: v,
        pf_kind :: PairFateKind v
    }





data PairFateKind v =     
        Incompatible
    |   NotAdjacent v
    |   OK VectorIndex

    deriving(Show)



newtype VerboseDD stepLog a = VerboseDD (WriterT (DList stepLog) (State VectorIndex) a)
    deriving(Functor,Applicative,Monad,MonadWriter (DList stepLog),MonadState VectorIndex)


nextIndex :: VerboseDD v VectorIndex
nextIndex = do
    i <- get
    put (succ i)
    return i


runVerboseDD :: VerboseDD stepLog d -> (d, [stepLog])
runVerboseDD (VerboseDD x) = second toList (evalState (runWriterT x) 0)


-- | Runs a VerboseDD computation which logs 'innerSteps' inside a computation which logs 'outerSteps'. The inner computation uses the same 'VectorIndex' supply as the outer one.
runVerboseDDSubWriter
  :: VerboseDD innerStep a -> VerboseDD outerStep (a, DList innerStep)
runVerboseDDSubWriter (VerboseDD x) = VerboseDD $ do
    lift (runWriterT x)
    

-- data VerboseDDVectorPairWithZeroSetIntersection v co w =
--     VerboseDDVectorPairWithZeroSetIntersection !v !v !(ZeroSet co w)


class (BitVector w, CoordSys co) => VerboseDDVectorRepresentation a co w | a -> co w where

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
    VG.find (liftM2 (&&) (ddrep_differentIndex x) (ddrep_differentIndex y))
                            $ findVectorsWithZeroSetAtLeast minZeroSet _Vp 

    where
        ddrep_differentIndex = (/=) `on` ddrep_index

intersectZeroSets :: VerboseDDVectorRepresentation a co w => a -> a -> ZeroSet co w
intersectZeroSets = bvIntersection `on` ddrep_zeroSet

instance (VerboseDDVectorRepresentation a co w, Show a) => Pretty (PairFate a) where
    pretty = ppPairFate pretty

ppPairFate
  :: VerboseDDVectorRepresentation a co w =>
     (PairFateKind a -> Doc) -> PairFate a -> Doc
ppPairFate ppTheKind (PairFate x y z) = 
        string "P" <> 
            (parens $ hsep (punctuate comma 
                [ pretty (ddrep_index x), pretty (ddrep_index y), ppTheKind z ]))

ppPairFateBrief
  :: VerboseDDVectorRepresentation a co w => PairFate a -> Doc
ppPairFateBrief = ppPairFate (ppPairFateKindBrief 0)

instance (VerboseDDVectorRepresentation a co w, Show a) => Show (PairFate a) where showsPrec = prettyShowsPrec




instance Show v => Pretty (PairFateKind v) where pretty = string . show

ppPairFateKindBrief
  :: (VerboseDDVectorRepresentation a co w) =>
    Int -- ^ prec 
    -> PairFateKind a -> Doc
ppPairFateKindBrief _ Incompatible = dullmagenta (text "Incompatible") 
ppPairFateKindBrief prec (NotAdjacent x) = 
    parensIf (prec > 10) (red (text "NotAdjacent") <+> prettyVI (ddrep_index x))

ppPairFateKindBrief prec (OK i) = 
    parensIf (prec > 10) (green (text "OK") <+> parens (char '*' <> prettyVI i))


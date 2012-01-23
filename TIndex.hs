{-# LANGUAGE DeriveGeneric, UndecidableInstances, FlexibleInstances, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module TIndex where

import Data.Word
import Language.Haskell.TH
import Util
import Test.QuickCheck
import Control.Applicative
import PrettyUtil -- Pretty Word orphan instance
import Quote
import ShortShow
import Data.Binary
import Data.Binary.Derive
import GHC.Generics(Generic)
import Control.Exception
import OrphanInstances()
import Language.Haskell.TH.Lift




-- | Tetrahedron index
newtype TIndex = TIndex Word
    deriving(Eq,Ord,Pretty,Enum,Num,Real,Integral,Binary)

tindex ::  Word -> TIndex
tindex = TIndex

-- -- | NOTE: Only fromInteger is supported (arithmetic doesn't make much sense on these)
-- instance Num TIndex where
--     fromInteger = tindex . fromIntegral
--     (+) = error ("(+) not supported for TIndex")
--     (*) = error ("(+) not supported for TIndex")
--     abs = error ("abs not supported for TIndex")
--     signum = error ("signum not supported for TIndex")

-- | Thing with a tetrahedron index attached to it
data I a = I TIndex a 
    deriving(Eq,Ord,Generic)

instance Binary a => Binary (I a) where
    put = derivePut
    get = deriveGet


-- | Instances of this class essentially say that @ia@ is isomorphic to @('TIndex',a)@ (but the representation is left open to for optimization)
class HasTIndex ia a | ia -> a, a -> ia where
    -- | Unpack some tetrahedron-indexed entity
    viewI :: ia -> I a
    -- | Attach a tetrahedron index to some entity
    (./) :: TIndex -> a -> ia

infix 9 ./


getTIndex ::  HasTIndex a a' => a -> TIndex
getTIndex (viewI -> I i _) = i

forgetTIndex ::  HasTIndex a a' => a -> a'
forgetTIndex (viewI -> I _ a) = a 

-- instance HasTIndex (I a) a where
--     viewI = id
--     (./) = I

instance Show TIndex where
    show (TIndex i) = show i

instance ShortShow TIndex where
    shortShow = show


trivialHasTIndexInstance :: Q Type -> Q [Dec]
trivialHasTIndexInstance = 
    fmap (\t -> 
    [InstanceD [] 
        (AppT (AppT (ConT ''HasTIndex) (AppT (ConT ''I) t)) t) 
        [ValD (VarP 'viewI) (NormalB (VarE 'id)) [],
         ValD (VarP '(./)) (NormalB (ConE 'I)) []]])

mapI
  :: (HasTIndex ia a, HasTIndex ib b) => (a -> b) -> ia -> ib
mapI f (viewI -> I i x) = i ./ f x 

traverseI
  :: (HasTIndex ia a, HasTIndex ib b) =>
     ((b -> ib) -> fa -> fib) -> (a -> fa) -> ia -> fib
traverseI map_ f (viewI -> I i x) = map_ (i ./) (f x) 

instance (Show a) => Show (I a) where 
--    showsPrec prec (I i x) = showParen (prec >= 1) (showsPrec 10 i . showString " ./ " . showsPrec 10 x)
    showsPrec _ (I i x) = shows i . showChar '.' . shows x
    
instance (ShortShow a) => ShortShow (I a) where 
    shortShowsPrec _ (I i x) = shortShows i . showChar '.' . shortShows x

instance Quote TIndex where
    quotePrec _ (TIndex i) = show i -- relies on Num TIndex instance

instance (Quote a) => Quote (I a) where 
    quotePrec prec (I i x) = quoteParen (prec > 9) (quotePrec 10 i ++ " ./ " ++ quotePrec 10 x)

instance Finite a => Enum (I a) where
    toEnum n = case toEnum n of EnumPair a b -> I a b
    fromEnum (I a b) = fromEnum (EnumPair a b)

instance (Pretty a) => Pretty (I a) where
    pretty (I x y) = 
        
        dullcyan (pretty x) <> dot <> pretty y

instance (Arbitrary a) => Arbitrary (I a) where
    arbitrary = I <$> arbitrary <*> arbitrary

instance Arbitrary TIndex where
    arbitrary = (tindex . fromIntegral :: Int -> TIndex) `fmap` 
                sized (\n -> choose (0,n))



unI :: HasTIndex ia a => ia -> a
unI (viewI -> I _ x) = x


newtype TwoSkeleton a = TwoSkeleton { unTwoSkeleton :: a }
newtype OneSkeleton a = OneSkeleton { unOneSkeleton :: a }
newtype ZeroSkeleton a = ZeroSkeleton { unZeroSkeleton :: a } 


withTIndexEqual
  :: (HasTIndex ia a, HasTIndex ib b , HasTIndex ic c) =>
     (( a,  b) ->  c) -> (ia,ib) -> ic
withTIndexEqual f 
    (
        (viewI -> I i0 v0) 
    ,   (viewI -> I i1 v1) 
    )

    =
        assert (i0==i1) $

        i0 ./ f (v0, v1)

withTIndexEqualC
  :: (HasTIndex ia a, HasTIndex ib b , HasTIndex ic c) =>
     ( a ->  b ->  c) -> ia -> ib -> ic
withTIndexEqualC = curry . withTIndexEqual . uncurry

inIOf :: (HasTIndex ia a, HasTIndex ib b) => b -> ia -> ib
inIOf b ia = getTIndex ia ./ b

deriveLiftMany [''TIndex,''I]

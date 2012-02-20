{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, DeriveDataTypeable, DeriveGeneric, UndecidableInstances, FlexibleInstances, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
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
import Data.Ix
import Control.Arrow
import System.Random
import THUtil
import Control.DeepSeq.TH
import Control.DeepSeq
import Data.Typeable
import Data.SumType




-- | Tetrahedron index
newtype TIndex = TIndex Word
    deriving(Eq,Ord,Pretty,Enum,Real,Integral,Binary,Ix,Random,NFData,Typeable)


instance Num TIndex where
    (+) (TIndex a) (TIndex b) = 
        let c = a+b
        in
            if a<=c && b<=c
               then TIndex (a+b)
               else $err' ("TIndex "++show a++ " + TIndex "++show b)

    (-) (TIndex a) (TIndex b) | a < b =
            $err' ("TIndex "++show a++ " - TIndex "++show b)

            | otherwise = TIndex (a-b)

    (*) = $err' ("* not supported for TIndex")
    abs = $err' ("abs not supported for TIndex")
    signum = $err' ("signum not supported for TIndex")

    fromInteger i | i < 0 || i > toInteger (maxBound :: Word) 
                        = $err' ("fromInteger "++show i++" :: TIndex")
                  | otherwise = TIndex (fromInteger i)

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
    deriving(Eq,Ord,Generic,Ix,Typeable)

instance Binary a => Binary (I a) where
    put = derivePut
    get = deriveGet


-- | Instances of this class essentially say that @ia@ is isomorphic to @('TIndex',a)@ (but the representation is left open to for optimization)
class MapTIndices ia => HasTIndex ia a | ia -> a, a -> ia where
    -- | Unpack some tetrahedron-indexed entity
    viewI :: ia -> I a
    -- | Attach a tetrahedron index to some entity
    (./) :: TIndex -> a -> ia

infix 9 ./


getTIndex ::  HasTIndex a a' => a -> TIndex
getTIndex (viewI -> I i _) = i

forgetTIndex ::  HasTIndex a a' => a -> a'
forgetTIndex (viewI -> I _ a) = a 

unviewI :: HasTIndex ia a => I a -> ia
unviewI (I i a)= i ./ a

-- instance HasTIndex (I a) a where
--     viewI = id
--     (./) = I

instance Show TIndex where
    showsPrec = prettyShowsPrec


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
    showsPrec _ (I i x) = shows i . showString "./" . shows x
    
instance (ShortShow a) => ShortShow (I a) where 
    shortShowsPrec _ (I i x) = shortShows x . shortShows i

instance Quote TIndex where
    quotePrec _ (TIndex i) = show i

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

class MapTIndices a where
    mapTIndices :: (TIndex -> TIndex) -> (a -> a)

    mapTIndicesStrictlyMonotonic :: (TIndex -> TIndex) -> (a -> a)
    mapTIndicesStrictlyMonotonic = mapTIndices

instance MapTIndices (I a) where
    mapTIndices f (I i x) = I (f i) x

instance MapTIndices TIndex where
    mapTIndices = id


-- | Default 'mapTIndices' implementation for 'HasTIndex' instances. 
mapTIndicesI :: HasTIndex ia a => (TIndex -> TIndex) -> ia -> ia
mapTIndicesI f = unviewI . mapTIndices f . viewI

mapTIndicesFromHasTIndex :: TypeQ -> Q [Dec]
mapTIndicesFromHasTIndex t = [d| instance MapTIndices $(t) where mapTIndices = mapTIndicesI |]

deriveLiftMany [''TIndex,''I]

instance (MapTIndices a, MapTIndices b) => MapTIndices (a,b) where
    mapTIndices f = mapTIndices f *** mapTIndices f

deriveNFData ''I


mapTIndices_sum
  :: (SuperSumTy r,
      SubSumTy ab,
      MapTIndices (L ab),
      MapTIndices (R ab),
      R r ~ R ab,
      L r ~ L ab) =>
     (TIndex -> TIndex) -> ab -> r
mapTIndices_sum f = mapTIndices f ++++ mapTIndices f


viewI_sum
  :: (SuperSumTy a,
      SubSumTy ab,
      HasTIndex (L ab) (L a),
      HasTIndex (R ab) (R a)) =>
     ab -> I a
viewI_sum =
 either' 
            (\(viewI -> I i x) -> I i (left' x))
            (\(viewI -> I i x) -> I i (right' x)) 


addTIndex_sum
  :: (SuperSumTy r,
      SubSumTy ab,
      HasTIndex (L r) (L ab),
      HasTIndex (R r) (R ab)) =>
     TIndex -> ab -> r
addTIndex_sum i y = either' (left' . (i ./)) (right' . (i ./)) y


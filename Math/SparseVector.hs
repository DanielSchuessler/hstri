{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, TypeSynonymInstances, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module Math.SparseVector where

import Control.Applicative
import Data.Foldable(Foldable)
import qualified Data.Map as M
import Data.Map(Map)
import Data.Maybe
import Data.Monoid
import Data.VectorSpace
import Test.QuickCheck
import qualified Data.Foldable as Fold
import qualified Data.List as L
import Data.String
import Data.Function
import PrettyUtil
import Data.Traversable(Traversable)
import OrphanInstances() -- NFData Map
import Control.DeepSeq
import Data.Typeable


isZero :: (Eq a, Num a) => a -> Bool
isZero = (==0)

zero :: Num a => a
zero = 0

-- | Represents a function @k -> r@ with all but finitely many values being zero.
newtype SparseVector k r = SparseV { illdefinedSparseToMap :: Map k r }
    deriving(Foldable,Functor,Traversable,NFData,Typeable)

instance (Eq r, Num r, Ord k) => Eq (SparseVector k r) where
    (==) = (==) `on` (illdefinedSparseToMap . sparse_normalize)
    
instance (Ord r, Num r, Ord k) => Ord (SparseVector k r) where
    compare = compare `on` (illdefinedSparseToMap . sparse_normalize)

sparse_under
  :: (Map t t1 -> Map k r)
     -> SparseVector t t1 -> SparseVector k r
sparse_under f (SparseV m) = SparseV (f m)

sparse_under2
  :: (Map t t1 -> Map t2 t3 -> Map k r)
     -> SparseVector t t1
     -> SparseVector t2 t3
     -> SparseVector k r
sparse_under2 f (SparseV m) (SparseV m') = SparseV (f m m')

-- | Removes all zero-valued entries from the underlying 'Map'.
--
-- Property: @sparse_normalize m == m@
sparse_normalize
  :: (Num r, Ord k, Eq r) => SparseVector k r -> SparseVector k r
sparse_normalize = sparse_under (M.filter (not . isZero))

mapNonZeroing
  :: (t1 -> r) -> SparseVector k t1 -> SparseVector k r
mapNonZeroing = sparse_under . M.map

sparse_get :: (Num r, Ord k, Eq r) => SparseVector k r -> k -> r
sparse_get (SparseV m) k = fromMaybe zero (M.lookup k m) 

sparse_zero :: Ord k => SparseVector k r
sparse_zero = SparseV mempty

-- | = 'sparse_zero'
sparse_empty :: Ord k => SparseVector k r
sparse_empty = sparse_zero

-- | The given function must have zero as a neutral element, or else this won't be independent of the representation
sparse_addWith :: Ord k => (r -> r -> r)
     -> SparseVector k r -> SparseVector k r -> SparseVector k r
sparse_addWith f = sparse_under2 (M.unionWith f)

instance (Ord k, Num r) => AdditiveGroup (SparseVector k r) where
    zeroV = sparse_zero
    (^+^) = sparse_addWith (+)

    negateV = mapNonZeroing negate

instance (Ord k, Num r, Eq r) => VectorSpace (SparseVector k r) where
    type Scalar (SparseVector k r) = r
    0 *^ _ = zeroV
    r *^ x = mapNonZeroing (r*) x 

instance (Ord k, Num r, Eq r) => InnerSpace (SparseVector k r) where
    SparseV x <.> SparseV y = Fold.foldl' (+) 0 (M.intersectionWith (*) x y)


sparse_singleton :: (Num r, Ord k, Eq r) => k -> r -> SparseVector k r
sparse_singleton k r 
    | isZero r = zeroV
    | otherwise = SparseV (M.singleton k r)

sparse_fromAssocs
  :: (Functor f, Num r, Ord k, Foldable f, Eq r) =>
     f (k, r) -> SparseVector k r
sparse_fromAssocs = sumV . fmap (uncurry sparse_singleton)

-- | May contain zeroes, but not repetitions.
sparse_toAssocs :: SparseVector k a -> [(k, a)]
sparse_toAssocs = M.assocs . illdefinedSparseToMap

sparse_toAscList :: SparseVector k a -> [(k, a)]
sparse_toAscList = M.toAscList . illdefinedSparseToMap

sparse_fromDistinctAscList
  :: (Num r, Ord k, Eq r) => [(k, r)] -> SparseVector k r
sparse_fromDistinctAscList = sparse_fromMap . M.fromDistinctAscList

sparse_map
  :: (Num r', Ord k, Eq r') =>
     (r -> r') -> SparseVector k r -> SparseVector k r'
sparse_map f = sparse_mapWithKey (const f)


sparse_mapWithKey
  :: (Num r', Ord k, Eq r') =>
     (k -> r -> r') -> SparseVector k r -> SparseVector k r'
sparse_mapWithKey f (SparseV m) = 
    SparseV $
        M.mapMaybeWithKey (\k r -> let fkr = f k r
                                   in if isZero fkr 
                                      then Nothing 
                                      else Just fkr) 
                   m


--sparse_bindWithKey f m =

sparse_fromMap :: (Num r, Ord k, Eq r) => Map k r -> SparseVector k r
sparse_fromMap = SparseV

sparse_isZero :: (Num r, Ord k, Eq r) => SparseVector k r -> k -> Bool
sparse_isZero (SparseV m) k = maybe True isZero (M.lookup k m)

sparse_set
  :: (Num a, Ord k, Eq a) => k -> a -> SparseVector k a -> SparseVector k a
sparse_set k r (SparseV m) = 
  SparseV $ case () of
    _ | isZero r  -> M.delete k m
      | otherwise -> M.insert k r m 


sparse_adjust
  :: Ord k => (r -> r) -> k -> SparseVector k r -> SparseVector k r
sparse_adjust k f (SparseV m) =
    SparseV (M.adjust k f m)


sparse_gen
  :: (Num r, Ord k, Arbitrary r, Eq r) => [k] -> Gen (SparseVector k r)
sparse_gen keys_ = sparse_fromAssocs <$> listOf ((,) <$> elements keys_ <*> arbitrary)

sparse_toNonzeroAssocs :: (Eq b, Num b) => SparseVector a b -> [(a, b)]
sparse_toNonzeroAssocs = L.filter (not . isZero . snd) . sparse_toAssocs

instance (Show k, Show r, Num r, Ord r) => Show (SparseVector k r) where

    showsPrec prec sparse = 
        showParen (prec > 10)
            (showString "sparse_fromDistinctAscList "
                . showsPrec 11 (sparse_toAscList sparse))

        
--         showParen (prec > 6) $
--             sparse_showWith showString id (.) (showsPrec 11) (showsPrec 11) sparse


instance (Pretty k, Pretty r, Num r, Ord r) => Pretty (SparseVector k r) where
    prettyPrec prec sparse = 
        parensIf (prec > 6) $
            sparse_showWith text mempty mappend (prettyPrec 11) (prettyPrec 11) sparse

sparse_showWith
  :: (Num r, Ord r) =>
     ([Char] -> str)
     -> str
     -> (str -> str -> str)
     -> (r -> str)
     -> (k -> str)
     -> SparseVector k r
     -> str
sparse_showWith showString_ empty_ append_ sp11_r sp11_k sparse = 

            case sparse_toNonzeroAssocs sparse of
                 [] -> showChar_ '0'
                 [kr] -> s True kr 
                 kr0:rest -> s True kr0 `append_` foldr (\kr -> (s False kr `append_`)) empty_ rest

                

      where
        showChar_ = showString_ . return        

        s isFirst (k,r)
            | r < 0     =   showString_ " - " 
                            `append_` 
                            (if r== -1 
                                then empty_ 
                                else sp11_r (-r) `append_` showChar_ ' ') 
                            `append_` 
                            sp11_k k

            | otherwise = (if isFirst 
                              then empty_
                              else showString_ " + ") 
                              
                                `append_` 
                                (if r==1 
                                    then empty_
                                    else sp11_r r `append_` showChar_ ' ') 
                                `append_` 
                                sp11_k k


newtype Variable = Variable { variableName :: String }
    deriving(Eq,Ord,IsString,Typeable)

variable :: String -> Variable
variable = Variable

instance Show Variable where show = variableName

-- | '*', 'abs', 'signum', and @'fromInteger' i@ for @i /= 0@ are not supported!
instance (Ord r, Show k, Ord k, Num r) => Num (SparseVector k r) where

    fromInteger 0 = zeroV
    fromInteger _ = error ("fromInteger n not supported for SparseVector and n /= 0")

    (+) = (^+^)

    (*) = error ("(*) not supported for SparseVector")
    abs = error ("abs not supported for SparseVector")
    signum = error ("signum not supported for SparseVector")





sparse_sumWith
  :: Ord k => (r -> r -> r) -> [SparseVector k r] -> SparseVector k r
sparse_sumWith f (xs :: [SparseVector k r]) = SparseV (M.unionsWith f (L.map illdefinedSparseToMap xs))


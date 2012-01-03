{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, TypeSynonymInstances, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module ZeroDefaultMap where

import Control.Applicative
import Data.Foldable(Foldable)
import Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.VectorSpace
import Test.QuickCheck
import qualified Data.Foldable as Fold
import qualified Data.List as L
import Data.String
import Data.Function
import PrettyUtil

isZero :: Num a => a -> Bool
isZero = (==0)

zero :: Num a => a
zero = 0

newtype ZeroDefaultMap k r = ZDM { zdm_toMap :: Map k r }
    deriving(Foldable)

instance (Eq r, Num r, Ord k) => Eq (ZeroDefaultMap k r) where
    (==) = (==) `on` (zdm_toMap . zdm_normalize)
    
instance (Ord r, Num r, Ord k) => Ord (ZeroDefaultMap k r) where
    compare = compare `on` (zdm_toMap . zdm_normalize)

zdm_under
  :: (Map t t1 -> Map k r)
     -> ZeroDefaultMap t t1 -> ZeroDefaultMap k r
zdm_under f (ZDM m) = ZDM (f m)
zdm_under2
  :: (Map t t1 -> Map t2 t3 -> Map k r)
     -> ZeroDefaultMap t t1
     -> ZeroDefaultMap t2 t3
     -> ZeroDefaultMap k r
zdm_under2 f (ZDM m) (ZDM m') = ZDM (f m m')

zdm_normalize
  :: (Num r, Ord k) => ZeroDefaultMap k r -> ZeroDefaultMap k r
zdm_normalize = zdm_under (M.filter (not . isZero))

mapNonZeroing
  :: (t1 -> r) -> ZeroDefaultMap k t1 -> ZeroDefaultMap k r
mapNonZeroing = zdm_under . M.map

zdm_get :: (Num r, Ord k) => ZeroDefaultMap k r -> k -> r
zdm_get (ZDM m) k = fromMaybe zero (M.lookup k m) 

-- | Absent keys are taken to have coefficient zero
instance (Ord k, Num r) => AdditiveGroup (ZeroDefaultMap k r) where
    zeroV = ZDM mempty
    ZDM xs ^+^ ZDM ys = ZDM (unionWith (+) xs ys)

    negateV = mapNonZeroing negate

-- | Absent keys are taken to have coefficient zero
instance (Ord k, Num r) => VectorSpace (ZeroDefaultMap k r) where
    type Scalar (ZeroDefaultMap k r) = r
    0 *^ _ = zeroV
    r *^ x = mapNonZeroing (r*) x 

-- | Absent keys are taken to have coefficient zero
instance (Ord k, Num r) => InnerSpace (ZeroDefaultMap k r) where
    ZDM x <.> ZDM y = Fold.foldl' (+) 0 (intersectionWith (*) x y)


zdm_singleton :: (Num r, Ord k) => k -> r -> ZeroDefaultMap k r
zdm_singleton k r 
    | isZero r = zeroV
    | otherwise = ZDM (M.singleton k r)

zdm_fromAssocs
  :: (Functor f, Num r, Ord k, Foldable f) =>
     f (k, r) -> ZeroDefaultMap k r
zdm_fromAssocs = sumV . fmap (uncurry zdm_singleton)

zdm_toAssocs :: ZeroDefaultMap k a -> [(k, a)]
zdm_toAssocs = M.assocs . zdm_toMap

zdm_map
  :: (Num r', Ord k) =>
     (r -> r') -> ZeroDefaultMap k r -> ZeroDefaultMap k r'
zdm_map f = zdm_mapWithKey (const f)


zdm_mapWithKey
  :: (Num r', Ord k) =>
     (k -> r -> r') -> ZeroDefaultMap k r -> ZeroDefaultMap k r'
zdm_mapWithKey f (ZDM m) = 
    ZDM $
        M.mapMaybeWithKey (\k r -> let fkr = f k r
                                   in if isZero fkr 
                                      then Nothing 
                                      else Just fkr) 
                   m


--zdm_bindWithKey f m =

zdm_fromMap :: (Num r, Ord k) => Map k r -> ZeroDefaultMap k r
zdm_fromMap = ZDM

zdm_isZero :: (Num r, Ord k) => ZeroDefaultMap k r -> k -> Bool
zdm_isZero (ZDM m) k = maybe True isZero (M.lookup k m)

zdm_set
  :: (Num a, Ord k) => k -> a -> ZeroDefaultMap k a -> ZeroDefaultMap k a
zdm_set k r (ZDM m) = 
  ZDM $ case () of
    _ | isZero r  -> M.delete k m
      | otherwise -> M.insert k r m 


zdm_gen
  :: (Num r, Ord k, Arbitrary r) => [k] -> Gen (ZeroDefaultMap k r)
zdm_gen keys_ = zdm_fromAssocs <$> listOf ((,) <$> elements keys_ <*> arbitrary)

zdm_toNonzeroAssocs :: (Eq b, Num b) => ZeroDefaultMap a b -> [(a, b)]
zdm_toNonzeroAssocs = L.filter (not . isZero . snd) . zdm_toAssocs

instance (Show k, Show r, Num r, Ord r) => Show (ZeroDefaultMap k r) where

    showsPrec prec zdm = 
        showParen (prec > 6) $
            zdm_showWith showString id (.) (showsPrec 11) (showsPrec 11) zdm

zdm_showWith
  :: (Num r, Ord r) =>
     ([Char] -> str)
     -> str
     -> (str -> str -> str)
     -> (r -> str)
     -> (k -> str)
     -> ZeroDefaultMap k r
     -> str
zdm_showWith showString_ empty_ append_ sp11_r sp11_k zdm = 

            case zdm_toNonzeroAssocs zdm of
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
    deriving(Eq,Ord,IsString)

variable :: String -> Variable
variable = Variable

instance Show Variable where show = variableName

instance (Ord r, Show k, Ord k, Num r) => Num (ZeroDefaultMap k r) where

    fromInteger 0 = zeroV
    fromInteger _ = error ("fromInteger n not supported for ZeroDefaultMap and n /= 0")

    (+) = (^+^)

    (*) = error ("(*) not supported for ZeroDefaultMap")
    abs = error ("abs not supported for ZeroDefaultMap")
    signum = error ("signum not supported for ZeroDefaultMap")



instance (Show k, Show r, Num r, Ord r) => Pretty (ZeroDefaultMap k r) where
    prettyPrec = prettyPrecFromShow

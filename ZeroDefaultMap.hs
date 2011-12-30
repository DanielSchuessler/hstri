{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction, TypeSynonymInstances, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module ZeroDefaultMap where

import Collections(unionMaybeWith)
import Data.VectorSpace
import Data.Monoid
import qualified Data.Foldable as Fold
import Data.Map as M
import Data.Maybe
import Control.Exception
import Data.Foldable(Foldable)

newtype ZeroDefaultMap k r = ZDM { zdm_toMap :: Map k r }
    deriving(Eq,Ord,Foldable)

mapNonZeroing
  :: (r -> r') -> ZeroDefaultMap k r -> ZeroDefaultMap k r'
mapNonZeroing f (ZDM m) = ZDM (fmap f m)

zdm_get :: (Num r, Ord k) => ZeroDefaultMap k r -> k -> r
zdm_get (ZDM m) k = fromMaybe 0 (M.lookup k m) 

-- | Absent keys are taken to have coefficient zero
instance (Ord k, Num r) => AdditiveGroup (ZeroDefaultMap k r) where
    zeroV = ZDM mempty
    ZDM xs ^+^ ZDM ys = ZDM (unionMaybeWith f xs ys)
        where
            f x y = case x + y of
                         0 -> Nothing
                         s -> Just s
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
    | r == 0 = zeroV
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
                                   in if fkr == 0 then Nothing else Just fkr) 
                   m


--zdm_bindWithKey f m =

zdm_fromMap :: (Num r, Ord k) => Map k r -> ZeroDefaultMap k r
zdm_fromMap = ZDM . M.filter (/= 0)

zdm_isZero :: (Num r, Ord k) => ZeroDefaultMap k r -> k -> Bool
zdm_isZero (ZDM m) k = maybe True (\r -> assert (r/=0) False) (M.lookup k m)

zdm_set
  :: (Num a, Ord k) => k -> a -> ZeroDefaultMap k a -> ZeroDefaultMap k a
zdm_set k r (ZDM m) = 
  ZDM $ case () of
    _ | r == 0    -> M.delete k m
      | otherwise -> M.insert k r m 



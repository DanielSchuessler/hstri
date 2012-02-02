{-# LANGUAGE TypeOperators, TypeFamilies, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-} -- for emulated constraint synonym 'SumType'
module Data.SumType where
import Control.Compose
import Control.Arrow
import Control.Exception
import Data.Lens.Common
import Control.Comonad.Store

#define YES_CONSTRAINT_SYNONYM_INSTANCES

type family L ab
type family R ab

type instance L (Either a b) = a
type instance R (Either a b) = b

-- | @either_a_b@ contains the sum of @a@ and @b@
class SuperSumTy ab where
    left' :: L ab -> ab 
    right' :: R ab -> ab 

-- | @ab@ is contained in the sum of @a@ and @b@
class SubSumTy ab where
    either' :: (L ab -> r) -> (R ab -> r) -> (ab -> r)

class (SuperSumTy ab, SubSumTy ab) => SumType ab

instance (SuperSumTy ab, SubSumTy ab) => SumType ab  

instance SuperSumTy (Either a b) where
    left' = Left
    right' = Right

instance SubSumTy (Either a b) where
    either' = either

toEither :: SubSumTy ab => ab -> Either (L ab) (R ab)
toEither = either' Left Right

fromEither :: SuperSumTy ab => Either (L ab) (R ab) -> ab
fromEither = either left' right' 

(||||) = either'

(++++) kl kr = either' (left' . kl) (right' . kr) 

infixr 2 ++++, ||||

isLeft = either' (const True) (const False)

isRight = either' (const False) (const True)


fromRight :: (Show (L ab), SubSumTy ab) => ab -> R ab
fromRight = either' (error . f) id
    where
        f x = "fromRight called on a left: "++show x

bindSumTy x f = either' left' f x 

class (SubSumTy ab, SuperSumTy ab', L ab ~ L ab') =>
    MapRightSum ab ab'

#ifndef NO_CONSTRAINT_SYNONYM_INSTANCES
instance (SubSumTy ab, SuperSumTy ab', L ab ~ L ab') =>
    MapRightSum ab ab'
#endif

mapRight :: MapRightSum preMapRight postMapRight => 
    (R preMapRight -> R postMapRight) -> preMapRight -> postMapRight
mapRight f = id ++++ f



class (SubSumTy ab, SuperSumTy a'b, R ab ~ R a'b) =>
    MapLeftSum ab a'b

#ifndef NO_CONSTRAINT_SYNONYM_INSTANCES
instance (SubSumTy ab, SuperSumTy a'b, R ab ~ R a'b) =>
    MapLeftSum ab a'b
#endif

mapLeft
  :: (SubSumTy preMapLeft, SuperSumTy postMapLeft, R postMapLeft ~ R preMapLeft) =>
     (L preMapLeft -> L postMapLeft) -> preMapLeft -> postMapLeft
mapLeft f = f ++++ id

joinSumTy = flip bindSumTy id

class (SubSumTy preFlip, SuperSumTy postFlip, L preFlip ~ R postFlip, R preFlip ~ L postFlip)
        => FlippableSum preFlip postFlip

#ifndef NO_CONSTRAINT_SYNONYM_INSTANCES
instance (SubSumTy preFlip, SuperSumTy postFlip, L preFlip ~ R postFlip, R preFlip ~ L postFlip)
        => FlippableSum preFlip postFlip
#endif

flipSumTy :: FlippableSum preFlip postFlip => preFlip -> postFlip
flipSumTy = right' |||| left'


type LL ab = L (L ab)
type LR ab = L (R ab)
type RR ab = R (R ab)
type RL ab = R (L ab)

class 
     (
      LR rassoc ~ RL lassoc,
      L rassoc ~ LL lassoc,
      RR rassoc ~ R lassoc
     )

        => AssocableSumsBase lassoc rassoc
                 
#ifndef NO_CONSTRAINT_SYNONYM_INSTANCES
instance 
     (
      LR rassoc ~ RL lassoc,
      L rassoc ~ LL lassoc,
      RR rassoc ~ R lassoc
     )

        => AssocableSumsBase lassoc rassoc
#endif

class
     (SubSumTy rassoc,
      SubSumTy (R rassoc),
      SuperSumTy lassoc,
      SuperSumTy (L lassoc),
      AssocableSumsBase lassoc rassoc
      ) =>

          LAssocableSums lassoc rassoc

#ifndef NO_CONSTRAINT_SYNONYM_INSTANCES
instance
     (SubSumTy rassoc,
      SubSumTy (R rassoc),
      SuperSumTy lassoc,
      SuperSumTy (L lassoc),
      AssocableSumsBase lassoc rassoc
      ) =>

          LAssocableSums lassoc rassoc
#endif

class 
     (SubSumTy lassoc,
      SubSumTy (L lassoc),
      SuperSumTy (R rassoc),
      SuperSumTy rassoc,
      AssocableSumsBase lassoc rassoc
      ) =>

        RAssocableSums lassoc rassoc 

#ifndef NO_CONSTRAINT_SYNONYM_INSTANCES
instance 
     (SubSumTy lassoc,
      SubSumTy (L lassoc),
      SuperSumTy (R rassoc),
      SuperSumTy rassoc,
      AssocableSumsBase lassoc rassoc
      ) =>

        RAssocableSums lassoc rassoc 
#endif

-- | > a + (b + c) => (a + b) + c
lassocSumTy :: LAssocableSums lassoc rassoc => rassoc -> lassoc
lassocSumTy = (left' . left') |||| ((left' . right') |||| right')




-- | > (a + b) + c => a + (b + c)
rassocSumTy :: RAssocableSums lassoc rassoc => lassoc -> rassoc
rassocSumTy = (left' |||| (right' . left')) |||| (right' . right')

inLassoc
  :: (RAssocableSums lassocPost rassocPost,
      LAssocableSums lassocPre rassocPre) =>
     (lassocPre -> lassocPost) -> rassocPre -> rassocPost

inLassoc (f :: lassocPre -> lassocPost) (x :: rassocPre) = 
    (\(y :: rassocPost) -> y) -- fixes type variable name 
    (rassocSumTy (f (lassocSumTy x)))

inRassoc
  :: (RAssocableSums lassocPre rassocPre,
      LAssocableSums lassocPost rassocPost) =>
     (rassocPre -> rassocPost) -> lassocPre -> lassocPost

inRassoc (f :: rassocPre -> rassocPost) (x :: lassocPre) = 
    (\(y :: lassocPost) -> y) -- fixes type variable name  
    (lassocSumTy (f (rassocSumTy x)))

-- | > (a + b) + (c + d) => (a + c) + (b + d)
-- exchangeSum
--   :: forall 
--             a b c d 
--             ab ac bc cb cd bd
--             ac_b__d ab_c__d 
--             a_cb 
--             a_bc 
--             ab_c 
--             ac_b 
--             ac_bd 
--             ab_cd 
--             .
--      (RAssocableSums ac_b__d    ac_bd,
--       RAssocableSums ab_c       a_bc,
--       LAssocableSums ab_c__d    ab_cd,
--       LAssocableSums ac_b       a_cb,
--       FlippableSum bc cb,
--       MapLeftSum ab_c__d ac_b__d,
--       MapRightSum a_bc a_cb,
-- 
--       ab ~ L ab_cd,
--       cd ~ R ab_cd,
-- 
--       ac ~ L ac_bd,
--       bd ~ R ac_bd,
-- 
--       ab_c  ~ L ab_c__d,
--       d     ~ R ab_c__d,
-- 
--       ac_b  ~ L ac_b__d,
--       d     ~ R ac_b__d,
-- 
--       ab    ~ L ab_c,
--       c     ~ R ab_c,
-- 
--       ac    ~ L ac_b,
--       b     ~ R ac_b,
-- 
--       a     ~ L a_bc,
--       bc    ~ R a_bc,
-- 
--       a     ~ L a_cb,
--       cb    ~ R a_cb,
-- 
--       a ~ L ab,
--       b ~ R ab,
-- 
--       a ~ L ac,
--       c ~ R ac,
-- 
--       b ~ L bc,
--       c ~ R bc,
-- 
--       c ~ L cb,
--       b ~ R cb,
-- 
--       c ~ L cd,
--       d ~ R cd,
-- 
--       b ~ L bd,
--       d ~ R bd
-- 
--      ) =>
--      ab_cd -> ac_bd


exchangeSum
  :: (SubSumTy (L ab_cd),
      SubSumTy (R ab_cd),
      SubSumTy ab_cd,
      SuperSumTy ac_bd,
      SuperSumTy (L ac_bd),
      SuperSumTy (R ac_bd),
      R (R ab_cd) ~ R (R ac_bd),
      L (R ac_bd) ~ R (L ab_cd),
      L (R ab_cd) ~ R (L ac_bd),
      L (L ac_bd) ~ L (L ab_cd)) =>
     ab_cd -> ac_bd
exchangeSum =     fromEither . (fromEither +++ fromEither) 
                . go 
                . (toEither +++ toEither) . toEither 

    where
        go = inLassoc f

        f :: Either (Either (Either a b) c) d -> Either (Either (Either a c) b) d
        f = mapLeft g

        g :: Either (Either a b) c -> Either (Either a c) b
        g = inRassoc h 
        
        h :: Either a (Either b c) -> Either a (Either c b)
        h = mapRight i
        
        i :: Either b c -> Either c b
        i = flipSumTy

{-
 (a + b) + (c + d) => lassoc 
 ((a + b) + c) + d => rassoc + id
 (a + (b + c)) + d => (id + flip) + id
 (a + (c + b)) + d => lassoc + id
 ((a + c) + b) + d => rassoc
 (a + c) + (b + d)
-}


idEither = either Left Right

idEither'
  :: Either (Either a b) (Either c d)
     -> Either (Either a b) (Either c d)
idEither' = (idEither +++ idEither) . idEither

-- exchangeSum' :: ((Either a b) `Either` (Either c d)) -> ((Either a c) `Either` (Either b d))
-- exchangeSum' = exchangeSum


either2
  :: (SubSumTy ab, SubSumTy (L ab), SubSumTy (R ab)) =>
     (L (L ab) -> r)
     -> (R (L ab) -> r) -> (L (R ab) -> r) -> (R (R ab) -> r) -> ab -> r
either2 kll klr krl krr = either' (either' kll klr) (either' krl krr)



fromLeft' :: SubSumTy ab => ab -> L ab
fromLeft' = either' id (const $ assert False undefined)

fromRight' :: SubSumTy ab => ab -> R ab
fromRight' = either' (const $ assert False undefined) id


leftLens :: (SumType ab) => Lens (ab -> r) (L ab -> r)
leftLens = 
    Lens (\f ->
        store
            (\fl -> either' fl (f . right'))
            (f . left'))

rightLens :: (SumType ab) => Lens (ab -> r) (R ab -> r)
rightLens = 
    Lens (\f ->
        store
            (\fr -> either' (f . left') fr)
            (f . right'))

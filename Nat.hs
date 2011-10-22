{-# LANGUAGE UndecidableInstances, FlexibleInstances, ViewPatterns, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeOperators, DefaultSignatures, MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types, StandaloneDeriving #-}
-- {-# OPTIONS -Wall #-} 
module Nat(
    module TypeLevel.NaturalNumber, TNN.naturalNumberAsInt,
    module TypeLevel.NaturalNumber.Operations,
    module TypeLevel.NaturalNumber.Induction,
--    module Data.NaturalNumber,
    module Data.Type.Equality,
    HTrue,HFalse,HBool(..),IfThenElse,ifThenElse,
    HJust(..),hJust,HNothing,hNothing,
    HIsJust,DecidableEq(..),
    Pred,naturalNumberPred, IfEq,
    GetEqResult(..),

    induction',ShowN(..),showsPrecN,showN,OrdN(..),
    (:$),
    natEq, NaturalNumber(..),caseN2,caseN3,caseN4, S
    )
    where


import TypeLevel.NaturalNumber hiding(NaturalNumber(..))
import qualified TypeLevel.NaturalNumber as TNN
import TypeLevel.NaturalNumber.Operations
--import Data.NaturalNumber
import Data.Type.Equality
import TypeLevel.NaturalNumber.Induction
import Unsafe.Coerce

type S = SuccessorTo

class TNN.NaturalNumber n => NaturalNumber n where
    caseN :: forall r. 
        n -> 
        (n ~ Zero => r) -> 
        (forall pred. (NaturalNumber pred, n ~ S pred) => pred -> r) ->
        r

instance NaturalNumber Zero where
    caseN _ k0 _ = k0

instance NaturalNumber n => NaturalNumber (S n) where
    caseN _ _ k1 = k1 (undefined :: n)

caseN2 :: forall r n. NaturalNumber n => 
        n -> 
        (n ~ Zero => r) -> 
        (n ~ S Zero => r) -> 
        (forall pred. (NaturalNumber pred, n ~ S (S pred)) => pred -> r) ->
        r     
caseN2 n k0 k1 k2 = caseN n k0 (\n' -> caseN n' k1 k2)

caseN3 :: forall r n. NaturalNumber n => 
        n -> 
        (n ~ Zero => r) -> 
        (n ~ S Zero => r) -> 
        (n ~ S (S Zero) => r) -> 
        (forall pred. (NaturalNumber pred, n ~ S (S (S pred))) => pred -> r) ->
        r     
caseN3 n k0 k1 k2 k3 = caseN n k0 (\n' -> caseN2 n' k1 k2 k3)

caseN4 :: forall r n. NaturalNumber n => 
        n -> 
        (n ~ Zero => r) -> 
        (n ~ S Zero => r) -> 
        (n ~ S (S Zero) => r) -> 
        (n ~ S (S (S Zero)) => r) -> 
        (forall pred. (NaturalNumber pred, n ~ S (S (S (S pred)))) => pred -> r) ->
        r     
caseN4 n k0 k1 k2 k3 k4 = caseN n k0 (\n' -> caseN3 n' k1 k2 k3 k4)

-- destrN :: NaturalNumber n => n -> DestructN n
-- destrN = const destructN
-- 
-- 
-- data DestructN n =
--     n ~ Zero => Zero |
--     forall pred. n ~ SuccessorTo pred => Positive




data HTrue
data HFalse

class HBool a where
    caseBool :: a -> (a ~ HTrue => r) -> (a ~ HFalse => r) -> r

instance HBool HTrue where caseBool _ k0 _ = k0
instance HBool HFalse where caseBool _ _ k1 = k1

type family IfThenElse b t e
type instance IfThenElse HTrue t e = t
type instance IfThenElse HFalse t e = e

ifThenElse :: HBool b => b -> (b ~ HTrue => t) -> (b ~ HFalse => e) -> IfThenElse b t e
ifThenElse b t e = caseBool b t e


data HJust a = HJust a
    deriving Show

data HNothing

instance Show HNothing where
    show _ = "hNothing"

hJust = HJust

hNothing :: HNothing
hNothing = undefined 

data WMaybe a ma where
    WNothing :: WMaybe a HNothing
    WJust :: WMaybe a (HJust a)

deriving instance Show (WMaybe a ma)

class HMaybe a ma where
    wMaybe :: WMaybe a ma

instance HMaybe a HNothing where wMaybe = WNothing
instance HMaybe a (HJust a) where wMaybe = WJust

wMaybe' :: HMaybe a ma => ma -> WMaybe a ma
wMaybe' = const wMaybe

type family HIsJust t
type instance HIsJust (HJust a) = HTrue
type instance HIsJust HNothing = HFalse

-- hIsJust :: t -> HIsJust t
-- hIsJust _ = undefined

class HBool (DecideEq n n') => DecidableEq n n' where
    type DecideEq n n'
    getEq :: n -> n' -> GetEqResult n n'

data GetEqResult n n' where
    Equal :: (DecideEq n n ~ HTrue) => GetEqResult n n 
    NotEqual :: (DecideEq n n' ~ HFalse) => GetEqResult n n'

deriving instance Show (GetEqResult n n')

type IfEq a a' t e = IfThenElse (DecideEq a a') t e

decideEq :: n -> n' -> DecideEq n n'
decideEq _ _ = undefined


instance DecidableEq Zero Zero where
    type DecideEq Zero Zero = HTrue
    getEq _ _ = Equal

instance DecidableEq Zero (SuccessorTo n) where
    type DecideEq Zero (SuccessorTo n) = HFalse
    getEq _ _ = NotEqual

instance DecidableEq (SuccessorTo n) Zero where
    type DecideEq (SuccessorTo n) Zero = HFalse
    getEq _ _ = NotEqual

instance DecidableEq n n' => DecidableEq (SuccessorTo n) (SuccessorTo n') where
    type DecideEq (SuccessorTo n) (SuccessorTo n') = DecideEq n n'

    getEq (naturalNumberPred -> n)
          (naturalNumberPred -> n') = 

             case getEq n n' of
                  Equal -> Equal
                  NotEqual -> NotEqual




type family f :$ x
infixr 9 :$

data ApplyConstr (c :: * -> *)

type instance ApplyConstr c :$ x = c x 

data FMap f

type instance FMap f :$ HNothing = HNothing 
type instance FMap f :$ HJust a = HJust (f :$ a) 

data g :. f
infixr 9 :.

type instance (g :. f) :$ x = g :$ (f :$ x)

type instance FMap f :$ (a :=: b) = f :$ a :=: f :$ b


                    

type Pred n = Minus n N1

naturalNumberPred :: SuccessorTo n -> n
naturalNumberPred _ = undefined :: n

induction' :: forall f n. 
    f N0 -> 
    (forall n. NaturalNumber n => f n -> f (SuccessorTo n)) -> 
    NaturalNumber n => f n 
induction' b s = caseN (undefined :: n) 
                    b
                    (\_ -> s (induction' b s))

class ShowN a where
    getShow :: NaturalNumber n => a -> n -> (Show (a:$n) => r) -> r

class OrdN a where
    getOrd :: NaturalNumber n => a -> n -> (Ord (a:$n) => r) -> r


showsPrecN :: forall a n. (ShowN a, NaturalNumber n) => a -> n -> Int -> a:$n -> ShowS
showsPrecN a n = getShow a n showsPrec

showN :: (NaturalNumber n, ShowN a) => a -> n -> (a :$ n) -> String
showN a n x = showsPrecN a n 0 x ""

natEq :: (NaturalNumber n1, NaturalNumber n2) => n1 -> n2 -> Maybe (n1 :=: n2)
natEq n m = 
    caseN n 
        (caseN m (Just Refl) (const Nothing))
        (\n' -> caseN m Nothing (\m' -> fmap cong (natEq n' m')))
        


{-# LANGUAGE TemplateHaskell, DeriveFunctor, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses #-}
module Data.FiniteFunc where

import qualified Data.Map as M
import Data.Map(Map)
import EitherC
import Control.Exception
import qualified Data.Foldable as Fold
import Data.Foldable(Foldable,foldMap)
import qualified Data.Traversable as Trav
import Data.Traversable(Traversable,traverse,sequenceA)
import Control.Applicative
import Data.Function
import Control.Arrow
import qualified Data.Set as S
import Data.Set(Set)
import Data.Semigroupoid
import Control.Monad
import Data.Semigroupoid.Static
import Text.Groom

data FiniteFunc a b = FiniteFunc {
        ff_domain :: [a],
        -- | Should not return an error for elements of the domain
        ff_func :: a -> AttemptC b
    }
    deriving (Functor)

instance Foldable (FiniteFunc a) where
    foldMap f ff = foldMap (f . ff_apply ff) (ff_domain ff) 
    foldr c z ff = foldr (c . ff_apply ff) z (ff_domain ff) 

instance Ord a => Traversable (FiniteFunc a) where
    sequenceA ff = 
        fmap (
              ff_fromMapWithNotFoundHandler 
                (const (ErrorCall "FiniteFunc/sequenceA: Element not in the map")) 
            . M.fromList
            ) 
        . traverse (\a -> (a,) <$> ff_apply ff a) 
        . ff_domain
        $ ff

-- | Ignores the ordering of the domain
instance (Ord a, Eq b) => Eq (FiniteFunc a b) where
    (==) = (==) `on` ff_toMap

instance (Ord a, Ord b) => Ord (FiniteFunc a b) where
    compare = compare `on` ff_toMap

groomShowS = groomString . ($"")
    
instance (Show a, Show b) => Show (FiniteFunc a b) where
    showsPrec prec ff = 
        showString . groomShowS . showParen (prec>10)
            $ (showString "ff_fromAssocs " . showsPrec 11 (ff_toAssocs ff))

ff_lookup :: a -> FiniteFunc a b -> AttemptC b
ff_lookup = flip ff_func

-- | Partial
ff_apply :: FiniteFunc a b -> a -> b
ff_apply ff = $unEitherC "ff_apply" . ff_func ff  

ff_fromMapWithNotFoundHandler
  :: Ord k => (k -> ErrorCall) -> Map k b -> FiniteFunc k b
ff_fromMapWithNotFoundHandler h m = 
    FiniteFunc 
        (M.keys m) 
        (\x -> maybe ($failure' (h x)) return (M.lookup x m)) 

ff_fromMap :: Ord a => Map a b -> FiniteFunc a b
ff_fromMap = ff_fromMapWithNotFoundHandler (const (ErrorCall "ff_fromMap: Element not in the map")) 
    
ff_fromTotal :: [a] -> (a -> b) -> FiniteFunc a b
ff_fromTotal dom f = FiniteFunc dom (return . f)

ff_toMap :: Ord a => FiniteFunc a b -> Map a b
ff_toMap = M.fromList . ff_toAssocs

ff_toAssocs :: FiniteFunc a b -> [(a, b)]
ff_toAssocs ff = map (id &&& ff_apply ff) (ff_domain ff)

ff_fromAssocs :: Ord a => [(a, b)] -> FiniteFunc a b
ff_fromAssocs = ff_fromMap . M.fromList

ff_fromDistinctAscAssocs :: Ord a => [(a, b)] -> FiniteFunc a b
ff_fromDistinctAscAssocs = ff_fromMap . M.fromDistinctAscList  

ff_toDistinctAscAssocs :: Ord a => FiniteFunc a b -> [(a, b)]
ff_toDistinctAscAssocs = M.toAscList . ff_toMap

ff_codomain :: FiniteFunc a b -> [b]
ff_codomain ff = map (ff_apply ff) (ff_domain ff)

ff_domainSet :: Ord a => FiniteFunc a b -> Set a
ff_domainSet = S.fromList . ff_domain

ff_id :: [a] -> FiniteFunc a a
ff_id dom = ff_fromTotal dom id

ff_compose :: FiniteFunc b c -> FiniteFunc a b -> FiniteFunc a c
ff_compose  ff2 ff1 = FiniteFunc (ff_domain ff1) (ff_func ff2 <=< ff_func ff1) 

instance Semigroupoid FiniteFunc where
    o = ff_compose 



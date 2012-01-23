{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.ProductType where

type family Fst ab
type family Snd ab

class SubProdTy ab where
    prod :: Fst ab -> Snd ab -> ab

class SuperProdTy ab where
    fst' :: ab -> Fst ab
    snd' :: ab -> Snd ab

class (SubProdTy ab, SuperProdTy ab) => ProdTy ab
instance (SubProdTy ab, SuperProdTy ab) => ProdTy ab


-- class GSubProdTy f where
--     gprod :: Fst (f p) -> Snd (f p) -> f p



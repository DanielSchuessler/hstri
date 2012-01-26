{-# LANGUAGE BangPatterns, ScopedTypeVariables, TemplateHaskell, NoMonomorphismRestriction #-}
module Data.Numbering where

import Data.Map as M
import THUtil
import PrettyUtil
import Data.Maybe
import qualified Data.Vector as V
import Util

data Numbering a = Numbering {
    toInt :: a -> Int,
    fromInt :: Int -> a,
    nuLength :: Int
}

instance Show a => Show (Numbering a) where
    showsPrec prec nu = 
        showParen (prec > 10) 
            (showString "nuFromDistinctList " . showsPrec 11 (nuElements nu))

enumNu :: (Enum a) => a -> a -> Numbering a
enumNu min_ max_ = enumNu' (fromEnum min_) (fromEnum max_)

enumNu' :: Enum a => Int -> Int -> Numbering a
enumNu' mini maxi =
    Numbering {
        toInt = subtract mini . fromEnum
    ,   fromInt = toEnum . (+) mini
    ,   nuLength = maxi-mini+1

    }

sumNu
  :: (a1 -> a)
     -> (a2 -> a)
     -> ((a1 -> Int) -> (a2 -> Int) -> a -> Int)
     -> Numbering a1
     -> Numbering a2
     -> Numbering a
sumNu left_ right_ either_ nu1 nu2 = 
    let
        n1 = nuLength nu1
    in
        Numbering
            (either_ (toInt nu1) ((+ n1) . toInt nu2))
            (\i -> case i-n1 of
                        i' | i' < 0 -> left_ (fromInt nu1 i)
                           | otherwise -> right_ (fromInt nu2 i'))
            (n1+nuLength nu2)

eitherNu :: Numbering a -> Numbering b -> Numbering (Either a b)
eitherNu = sumNu Left Right either


prodNu
  :: (a -> a2)
     -> (a -> a1)
     -> (a2 -> a1 -> a)
     -> Numbering a2
     -> Numbering a1
     -> Numbering a
prodNu fst_ snd_ prod nu1 nu2 =
    let
        n2 = nuLength nu2
    in
        Numbering
            (\a -> toInt nu1 (fst_ a) * n2 + toInt nu2 (snd_ a))
            (\i -> case divMod i n2 of
                        (i1,i2) -> prod (fromInt nu1 i1) (fromInt nu2 i2)
            
            )
            (n2*nuLength nu1)
    


tupleNu :: Numbering a -> Numbering b -> Numbering (a, b)
tupleNu = prodNu fst snd (,)

nuIndices :: Numbering a -> [Int]
nuIndices nu = [0.. nuLength nu-1]

nuElements :: Numbering a -> [a]
nuElements nu = fmap (fromInt nu) (nuIndices nu)

checkNu :: Pretty a => Numbering a -> b -> b
checkNu nu r = 
    foldr (\i r -> let a_i = fromInt nu i
                       i_a_i = toInt nu a_i
                   in
                        $(assrt [|i == i_a_i|] ['i,'a_i,'i_a_i]) r)
          r
          (nuIndices nu)

    
-- | "Data.Set" doesn't expose the necessary index-based API
nuFromSet :: Map Int ignored -> Numbering Int
nuFromSet m = 
    Numbering
        (\i -> fst (elemAt i m))
        (\a -> fromMaybe
                    (error ("nuFromSet: Element not in Numbering: "++show a))
                    (lookupIndex a m)) 
        (size m)

nuFromDistinctVector :: forall a. (Show a, Ord a) => V.Vector a -> Numbering a
nuFromDistinctVector v =
    let
        m :: M.Map a Int 
        m = V.ifoldl' (\r i a -> M.insertWithKey _err a i r) M.empty v

        _err a i1 i2 = error ("nuFromDistinctVector: duplicate: " ++ show a++ " at indices "++show (i1,i2))
    in
        Numbering
            (\a -> fromMaybe
                        (error ("nuFromDistinctVector: Element not in Numbering: "++show a))
                        (M.lookup a m)) 
            (v V.!)
            (V.length v)

nuFromDistinctList :: (Ord a, Show a) => [a] -> Numbering a
nuFromDistinctList = nuFromDistinctVector . V.fromList 

nuFromList :: (Ord a, Show a) => [a] -> Numbering a
nuFromList = nuFromDistinctList . nub'

finiteTypeNu :: (Finite a) => Numbering a
finiteTypeNu = enumNu minBound maxBound


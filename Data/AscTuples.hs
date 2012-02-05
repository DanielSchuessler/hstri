{-# LANGUAGE GeneralizedNewtypeDeriving, CPP, NoMonomorphismRestriction, FlexibleInstances, FunctionalDependencies, DeriveDataTypeable, TypeFamilies, TemplateHaskell, DeriveGeneric, DeriveFoldable #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
-- | Description:  Strictly ascending tuples
module Data.AscTuples where

import Control.Arrow
import Control.Monad
import Data.Typeable
import Element
import HomogenousTuples
import Language.Haskell.TH
import PrettyUtil
import Math.Groups.S3
import ShortShow
import TupleTH
import Util
import qualified Data.Foldable as Fold
import DisjointUnion
import FaceClasses
import Language.Haskell.TH.Lift
import Data.Tuple.Index
import Data.Maybe

#define DEBUG

class (AsList asc, AsList tup, Element asc ~ Element tup) => 
    AscTuple tup asc | tup -> asc, asc -> tup where

    asc :: tup -> asc
    unAsc :: asc -> tup

    -- | Does not check orderedness
    unsafeAsc :: 
#ifdef DEBUG
        (Show tup) => 
#endif
    
        tup -> asc

$(flip concatMapM [2,3,4] (\i -> do

    let asciName = mkName ("Asc"++show i)
        ctorName = mkName ("UnsafeAsc"++show i)
        asciTy = conT asciName
        asciTy_a = asciTy `appT` aTy 

        aName = mkName "a"
        aTy = varT aName

    sequence 
     [
        newtypeD 
            (cxt [])
            asciName
            [PlainTV aName]
            (normalC ctorName [(strictType notStrict 
                                    (htuple i aTy))])
            [''Show,''Eq,''Ord,''Typeable,''Fold.Foldable,''ShortShow,''Pretty]

     , tySynInstD ''Element [asciTy_a] aTy 

     , instanceD (cxt[]) 
            (conT ''AsList `appT` asciTy_a)
            [valD (varP 'asList) (normalB (varE 'Fold.toList)) []]

     , instanceD 
            (cxt [classP ''Ord [aTy]]) 
            (conT ''AscTuple `appT` (htuple i aTy) `appT` asciTy_a)

            [valD (varP 'asc) (normalB (varE . mkName $ "asc"++show i)) []

            ,valD (varP 'unAsc) (normalB (varE . mkName $ "unAsc"++show i)) []

            ,valD (varP 'unsafeAsc) (normalB 
                
                [| \xs ->
                        let 
                            xs' = asc xs
                            xs'' = $(conE ctorName) xs
                        in
                            if (xs'/=xs'')
                               then error ("unsafeAsc "++show xs)
                               else xs''


                                |]
                
                
                ) []
            ] 

     ])
        


    )

asc2 :: Ord a => (Pair a) -> Asc2 a
asc2 as = case sort2 as of
                as'@(a',b') | a' < b' -> UnsafeAsc2 as'
                        | otherwise -> error ("asc2: arguments equal")

asc3 :: Ord a => (Triple a) -> Asc3 a
asc3 as = case sort3 as of
                as'@(a',b',c') 
                    | a' >= b' -> error ("asc3: smallest two arguments equal")
                    | b' >= c' -> error ("asc3: largest two arguments equal")
                    | otherwise -> UnsafeAsc3 as'

asc4 :: Ord a => (Quadruple a) -> Asc4 a
asc4 as = case sort4 as of
                as'@(a',b',c',d') 
                    | a' >= b' -> error ("asc4: smallest two arguments equal")
                    | b' >= c' -> error ("asc4: arguments not pairwise distinct")
                    | c' >= d' -> error ("asc4: largest two arguments equal")
                    | otherwise -> UnsafeAsc4 as'



unAsc2 :: Asc2 t -> Pair t
unAsc3 :: Asc3 t -> Triple t
unAsc4 :: Asc4 t -> Quadruple t

unAsc2 (UnsafeAsc2 as) = as
unAsc3 (UnsafeAsc3 as) = as
unAsc4 (UnsafeAsc4 as) = as

subtuplesAsc3_2 :: (Ord t1, Show t1) => Asc3 t1 -> Asc3 (Asc2 t1)
subtuplesAsc3_2 = unsafeAsc . map3 unsafeAsc . $(subtuples 3 2) . unAsc
subtuplesAsc4_3 :: (Ord t, Show t) => Asc4 t -> Asc4 (Asc3 t)
subtuplesAsc4_3 = unsafeAsc . map4 unsafeAsc . $(subtuples 4 3) . unAsc


sort3WithPermutation'
  :: (Ord t, Show t) => (t, t, t) -> (Asc3 t, S3)
sort3WithPermutation' = first unsafeAsc . sort3WithPermutation


isRegardedAsSimplexByDisjointUnionDeriving (conT ''Asc2 `appT` varT (mkName "v"))
isRegardedAsSimplexByDisjointUnionDeriving (conT ''Asc3 `appT` varT (mkName "v"))
isRegardedAsSimplexByDisjointUnionDeriving (conT ''Asc4 `appT` varT (mkName "v"))

instance Vertices (Asc2 v) where
    type Verts (Asc2 v) = Pair v
    vertices = unAsc2

instance Vertices (Asc3 v) where
    type Verts (Asc3 v) = Triple v
    vertices = unAsc3

instance Vertices (Asc4 v) where
    type Verts (Asc4 v) = Quadruple v
    vertices = unAsc4

instance Edges (Asc3 v) where
    type Eds (Asc3 v) = Triple (Asc2 v)
    edges = map3 UnsafeAsc2 . $(subtuples 3 2) . unAsc3

instance Triangles (Asc4 v) where
    type Tris (Asc4 v) = Quadruple (Asc3 v)
    triangles = map4 UnsafeAsc3 . $(subtuples 4 3) . unAsc4


indexOfAsc3 :: Ord a => a -> Asc3 a -> Maybe Index3
indexOfAsc3 x (UnsafeAsc3 (a, b, c)) =
    case compare x b of
         EQ -> Just I3_1 
         LT -> if x==a then Just I3_0 else Nothing
         GT -> if x==c then Just I3_2 else Nothing
         

elemAsc3 :: Ord a => a -> Asc3 a -> Bool
elemAsc3 = (.) isJust . indexOfAsc3


deriveLiftMany [''Asc2,''Asc3,''Asc4]

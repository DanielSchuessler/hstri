{-# LANGUAGE TupleSections, ScopedTypeVariables, GeneralizedNewtypeDeriving, CPP, NoMonomorphismRestriction, FlexibleInstances, FunctionalDependencies, DeriveDataTypeable, TypeFamilies, TemplateHaskell, DeriveGeneric, DeriveFoldable #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
-- | Description:  Strictly ascending tuples
module Data.AscTuples where

import Control.Applicative
import Data.Maybe
import Data.Tuple.Index
import Data.Typeable
import DisjointUnion
import EitherC
import Element
import FaceClasses
import HomogenousTuples
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Math.Groups.S3
import PrettyUtil
import QuickCheckUtil
import ShortShow
import Simplicial.DeltaSet3
import THBuild
import Test.QuickCheck
import TupleTH
import Util
import qualified Data.Foldable as Fold

#define DEBUG

class AscTuple1 asc where
    mapAscTotal :: Ord b => (a -> b) -> asc a -> EitherC LErrorCall (asc b)

    mapAscMonotonicTotal :: Ord b => (a -> b) -> asc a -> EitherC LErrorCall (asc b)
    mapAscMonotonicTotal = mapAscTotal


mapAsc :: (AscTuple1 asc, Ord b) => (a -> b) -> asc a -> asc b
mapAsc f = $unEitherC "mapAsc" . mapAscTotal f

mapAscMonotonic :: (AscTuple1 asc, Ord b) => (a -> b) -> asc a -> asc b
mapAscMonotonic f = $unEitherC "mapAscMonotonic" . mapAscMonotonicTotal f 

class (AscTuple1 asc, AsList (asc a), AsList tup, Element (asc a) ~ a, Element tup ~ a) => 
    AscTuple tup asc a | tup -> asc a, asc a -> tup where

    asc :: tup -> asc a
    ascTotal :: tup -> EitherC LErrorCall (asc a)
    unAsc :: asc a -> tup

    -- | Does not check orderedness
    unsafeAsc :: 
#ifdef DEBUG
        (Show tup) => 
#endif
    
        tup -> asc a



$(flip concatMapM [2,3,4] (\i -> do

    let asciName = mkName ("Asc"++show i)
        ctorName = mkName ("UnsafeAsc"++show i)
        asciTy = conT asciName
        asciTy_a = asciTy `appT` aTy 
        tupTy_a = htuple i aTy

        aName = mkName "a"
        aTy = varT aName

        unAsciName = mkName ("unAsc"++show i)

        smartCtorTotalName = mkName ("asc"++show i++"total")
        smartCtorName = mkName ("asc"++show i)

        forall_a = forallT [PlainTV aName]
        ord_a = classP ''Ord [aTy]


    sequence 
     [
        newtypeD 
            (cxt [])
            asciName
            [PlainTV aName]
            (normalC ctorName [strictType notStrict tupTy_a])
            [''Show,''Eq,''Ord,''Typeable,''Fold.Foldable,''ShortShow,''Pretty]

     , sigD  unAsciName (forall_a (cxt[]) [t| $asciTy_a -> $tupTy_a |]) 
     , valD' unAsciName (getFieldE ctorName 1 0)

     , sigD  smartCtorTotalName (forall_a (cxt[ord_a]) [t| $tupTy_a -> EitherC LErrorCall $asciTy_a |]) 
     , valD' smartCtorTotalName [|
            \as -> let as' = $(sortTuple i) as
                   in
                            case $(findSuccessiveElementsSatisfying i) (>=) as' of
                                 Nothing -> return ($(conE ctorName) as')
                                 Just _ -> $failureStr 
                                                (  $(lift (nameBase smartCtorTotalName))
                                                    ++": Elements not distinct")

         |]

     , sigD  smartCtorName (forall_a (cxt[ord_a]) [t| $tupTy_a -> $asciTy_a |]) 
     , valD' smartCtorName [| $unEitherC $(lift (nameBase smartCtorName)) . $(varE smartCtorTotalName) |]

     , tySynInstD ''Element [asciTy_a] aTy 

     , instanceD (cxt[]) 
            (''AsList `appT'` asciTy_a)
            [ valD' 'asList 'Fold.toList]

     , instanceD (cxt [])
            (''AscTuple1 `appT'` asciTy)
            [ valD' 'mapAscTotal 
                [| \f -> 
                        $(varE smartCtorTotalName) 
                        . $(mapTuple' i (varE 'f)) 
                        . $(varE unAsciName) 
                |]
            ]

     , instanceD 
            (cxt [classP ''Ord [aTy]]) 
            (''AscTuple `appT'` tupTy_a `appT` asciTy `appT` aTy)

            [valD' 'asc ((varE . mkName $ "asc"++show i))

            ,valD' 'ascTotal smartCtorTotalName

            ,valD' 'unAsc unAsciName

            ,valD' 'unsafeAsc (
                
                [| \xs ->
                        let 
                            xs' = asc xs
                            xs'' = $(conE ctorName) xs
                        in
                            if (xs'/=xs'')
                               then error ("unsafeAsc "++show xs)
                               else xs''


                                |]
                
                
                )
            ] 

     ])
        


    )



-- | Lexicographical order
subtuplesAsc3_2 :: (Ord t1, Show t1) => Asc3 t1 -> Asc3 (Asc2 t1)
subtuplesAsc3_2 = unsafeAsc . map3 unsafeAsc . subtuples3_2 . unAsc

-- | Lexicographical order
subtuplesAsc4_3 :: (Ord t, Show t) => Asc4 t -> Asc4 (Asc3 t)
subtuplesAsc4_3 = unsafeAsc . map4 unsafeAsc . subtuples4_3 . unAsc


sort3WithPermutation'
  :: (Ord a, Show a) =>
     (a, a, a) -> EitherC (LCommentedException LErrorCall) (Asc3 a, S3)
sort3WithPermutation' xs = 
    let (xs',g) = sort3WithPermutation xs 
    in
        (,g) <$>

        $(commentIfException) 
            ("sort3WithPermutation': Args not distinct: "++show xs) 
            (asc3total xs')

sort2WithPermutation'
  :: (Ord a, Show a) => (a, a) -> EitherC LErrorCall (Asc2 a, S2)
sort2WithPermutation' (x0,x1) =
    case compare x0 x1 of
         LT -> return (unsafeAsc (x0, x1),NoFlip)
         GT -> return (unsafeAsc (x1, x0),  Flip)

         _ -> $failureStr ("sort2WithPermutation': Args not distinct: "++show (x0,x1)) 


$(flip concatMapM [2,3,4] (\i ->
    isRegardedAsSimplexByDisjointUnionDeriving 
        (dimTName (i-1)) 
        (("Asc"++show i) `appT'` "v")
    ))

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

instance Edges (Asc4 v) where
    type Eds (Asc4 v) = Sextuple (Asc2 v)
    edges = map6 UnsafeAsc2 . $(subtuples 4 2) . unAsc4

instance Triangles (Asc4 v) where
    type Tris (Asc4 v) = Quadruple (Asc3 v)
    triangles = map4 UnsafeAsc3 . $(subtuples 4 3) . unAsc4

instance SatisfiesSimplicialIdentities2 (Asc3 v)
instance SatisfiesSimplicialIdentities3 (Asc4 v)

indexOfAsc3 :: Ord a => a -> Asc3 a -> Maybe Index3
indexOfAsc3 x (UnsafeAsc3 (a, b, c)) =
    case compare x b of
         EQ -> Just I3_1 
         LT -> if x==a then Just I3_0 else Nothing
         GT -> if x==c then Just I3_2 else Nothing
         

elemAsc3 :: Ord a => a -> Asc3 a -> Bool
elemAsc3 = (.) isJust . indexOfAsc3


deriveLiftMany [''Asc2,''Asc3,''Asc4]

instance (Ord v, Arbitrary v) => Arbitrary (Asc2 v) where arbitrary = generateUntilRight (asc2total <$> arbitrary)
instance (Ord v, Arbitrary v) => Arbitrary (Asc3 v) where arbitrary = generateUntilRight (asc3total <$> arbitrary)
instance (Ord v, Arbitrary v) => Arbitrary (Asc4 v) where arbitrary = generateUntilRight (asc4total <$> arbitrary)


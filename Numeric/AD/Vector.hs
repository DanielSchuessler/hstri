{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveTraversable, DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.AD.Vector where

import Control.Applicative
import Control.Monad
import Data.AdditiveGroup(AdditiveGroup(..))
import Data.Cross
import Data.Foldable(Foldable)
import Data.Foldable(Foldable,foldMap)
import Data.Function
import Data.List
import Data.Monoid(Sum(..))
import Data.Monoid(Sum(..))
import Data.Ord(comparing)
import Data.Traversable(Traversable)
import Data.Tuple.Index
import Data.Vect.Double.Base(Vec2(..),Vec3(..))
import Data.VectorSpace hiding(Sum(..))
import Data.VectorSpace(VectorSpace(..),InnerSpace(..))
import Language.Haskell.TH
import MathUtil
import Prelude hiding((<=))
import PrettyUtil(Pretty)
import THBuild
import THUtil
import TupleTH
import qualified Data.Foldable as Fold
import qualified Numeric.AD.Internal.Classes as AD
import qualified Prelude as P
import Util

innerProductForFoldableApplicative x y = getSum . foldMap Sum $ (liftA2 (*) x y) 

#define WANTED_DIMS [2,3,4]

$(liftM concat $ forM WANTED_DIMS (\n ->
    let
        na = mkName ("Tup"++show n)
        ctorname = na
        thetype = conT na
        a = mkName "a"

        n_vars stem = [ mkName ("_"++stem++show i) | i <- [0..n-1] ]
    in
        sequence
        [ snewtypeD (cxt[]) na (a&[]) (snormalC ctorname (htuple n (varT a))) 
            [''Functor,''Foldable,''Traversable,''Show,''Eq,''Ord ]

        , instanceD (cxt []) (''Applicative `sappT` thetype) 
            [
                svalD 'pure 
                    ("_x" \-> (ctorname `sappE` (stupE (replicate n "_x"))))
            ,   svalD '(<*>)
                    (sconP ctorname (stupP (n_vars "f")) \->
                     sconP ctorname (stupP (n_vars "x")) \->
                     sappE ctorname (tupE (zipWith sappE (n_vars "f") (n_vars "x"))))
            ]


        , svalD ("tup"++show n) (n_vars "x" \-> ctorname `sappE` (stupE (n_vars "x")))

        , svalD ("untup"++show n) (getFieldE ctorname 1 0)

        , svalD ("foldT"++show n) 
            (   "_k" \-> 
                sconP ctorname (stupP (n_vars "x")) \-> 
                foldl sappE (expQ "_k") (n_vars "x"))


        , instanceD 
            (cxt [sclassP ''Num "a"])
            (''AdditiveGroup `sappT` (thetype `sappT` "a"))
            [ svalD 'zeroV ('pure `sappE` integerL 0)
            , svalD '(^+^) ('liftA2 `sappE` '(+)) 
            , svalD 'negateV ('fmap `sappE` 'negate)
            ]

        , instanceD 
            (cxt [sclassP ''Num "a"])
            (''VectorSpace `sappT` (thetype `sappT` "a"))
            [ 
              stySynInstD ''Scalar [thetype `sappT` "a"] "a" 
            , svalD '(*^) ("p" \-> 'fmap `sappE` ("x" \-> '(*) `sappE` "p" `sappE` "x"))
            ]

        , instanceD 
            (cxt [sclassP ''Num "a"])
            (''InnerSpace `sappT` (thetype `sappT` "a"))
            [ 
              svalD '(<.>) 'innerProductForFoldableApplicative
            ]


        ]

    ))

-- $(concatMapM 
--     (\n -> 
--         inheritPretty 
--             (("Tup"++show n) `sappT` "a") 
--             (shtuple n "a")
--             ("untup"++show n))
--     WANTED_DIMS) 

tup2X ::  Num a => Tup2 a
tup2X = tup2 1 0
tup2Y ::  Num a => Tup2 a
tup2Y = tup2 0 1

tup3X ::  Num a => Tup3 a
tup3X = tup3 1 0 0
tup3Y ::  Num a => Tup3 a
tup3Y = tup3 0 1 0
tup3Z ::  Num a => Tup3 a
tup3Z = tup3 0 0 1

tup4X ::  Num a => Tup4 a
tup4X = tup4 1 0 0 0
tup4Y ::  Num a => Tup4 a
tup4Y = tup4 0 1 0 0
tup4Z ::  Num a => Tup4 a
tup4Z = tup4 0 0 1 0
tup4A ::  Num a => Tup4 a
tup4A = tup4 0 0 0 1

deriving instance Pretty a => Pretty (Tup2 a)
deriving instance Pretty a => Pretty (Tup3 a)
deriving instance Pretty a => Pretty (Tup4 a)

tup2toVec2 ::  Tup2 Double -> Vec2
tup2toVec2 = foldT2 Vec2
tup3toVec3 ::  Tup3 Double -> Vec3
tup3toVec3 = foldT3 Vec3

vec2toTup2 ::  Vec2 -> Tup2 Double
vec2toTup2 (Vec2 a b) = Tup2 (a, b)
vec3toTup3 ::  Vec3 -> Tup3 Double
vec3toTup3 (Vec3 a b c) = Tup3 (a, b, c)


-- | Consider a 'Vec2' as a constant
liftVec2 :: AD.Mode t => Vec2 -> Tup2 (t Double)
liftVec2 = fmap AD.lift . vec2toTup2 

-- | Consider a 'Vec3' as a constant
liftVec3 :: AD.Mode t => Vec3 -> Tup3 (t Double)
liftVec3 = fmap AD.lift . vec3toTup3 

{-# SPECIALIZE twoNormSqr :: Tup2 Double -> Double #-}
{-# SPECIALIZE twoNormSqr :: Tup3 Double -> Double #-}
twoNormSqr :: (Num c, Foldable t) => t c -> c
twoNormSqr = getSum . Fold.foldMap (\x-> Sum (x*x))

{-# SPECIALIZE twoNorm :: Tup2 Double -> Double #-}
{-# SPECIALIZE twoNorm :: Tup3 Double -> Double #-}
twoNorm :: (Floating c, Foldable t) => t c -> c
twoNorm = sqrt . twoNormSqr

-- -- Stolen from the 'vect' package
-- rotMatrix3normal :: Normal3 -> Flt -> Mat3
-- rotMatrix3normal (Tup3 x y z) a = 
--   let v = fromNormal u
--       c = cos a
--       s = sin a
--       m1 = scalarMul (1-c) (outer v v)
--       m2 = Mat3 (Vec3   c    ( s*z) (-s*y))
--                 (Vec3 (-s*z)   c    ( s*x))
--                 (Vec3 ( s*y) (-s*x)   c   )
--   in (m1 &+ m2)
-- 
--


-- | ARGS MUST BE OF LENGTH 1
slerpNormal
  :: (Eq (Scalar v), Floating (Scalar v), InnerSpace v) => Scalar v -> v -> v -> v
slerpNormal t p0 p1 =

    if s==0 then p0 else v


  where
    -- Stolen from the 'vect' package
  v = (p0 ^* y0) ^+^ (p1 ^* y1) 
  omega = acos (p0 <.> p1)
  s = sin omega
  y0 = sin (omega*(1-t)) / s 
  y1 = sin (omega*   t ) / s


-- | scale to unit sphere, slerp, then scale back (to the norm of the *first* arg)
slerpG
  :: (Floating uf,
      Ord uf,
      Foldable t,
      InnerSpace (t uf),
      Scalar (t uf) ~ uf,
      RealFloat uf,
      Show uf, Show (t uf) 
      ) =>
     uf -> t uf -> t uf -> t uf
slerpG 0 v0 _ = v0
slerpG 1 _ v1 = v1
slerpG t v0 v1 =
    let
        n0 = twoNorm v0
        n1 = twoNorm v1

        res = (slerpNormal t (v0 ^/ n0) (v1 ^/ n1)) ^* n0
    in
        if allReal res
        then res
        else error ("slerpG "++ sp11 t ++" " ++sp11 v0++" "++sp11 v1++" = "++show res)

allReal :: (RealFloat a, Foldable t) => t a -> Bool
allReal = not . Fold.any (liftM2 (||) isNaN isInfinite)

anyOrthT3 :: (Eq t1, Num t1) => Tup3 t1 -> Tup3 t1
anyOrthT3 = foldT3 (anyOrthG tup3)

interpolU :: Num a => a -> a -> a -> a
interpolU p x y = (1-p) * x + p * y

interpol :: (Num c, Applicative f) => c -> f c -> f c -> f c
interpol = liftA2 . interpolU

instance Num a => HasCross3 (Tup3 a) where
    cross3 = (.) Tup3 . (cross3 `on` untup3) 


twoNormalize
  :: (Eq s,
      Floating s,
      Show (t s),
      Foldable t,
      VectorSpace (t s),
      Scalar (t s) ~ s) =>
     t s -> t s
twoNormalize a =
    let
        n = twoNorm a
    in
        if n == 0
           then error ("twoNormalize "++show a)
           else a ^/ n 


isKindaUnitLength
  :: (Fractional a, Ord a, Foldable t) => t a -> Bool
isKindaUnitLength v = abs (twoNormSqr v - 1) < 1E-13

isKindaZero :: (Fractional a, Ord a, Foldable t) => t a -> Bool
isKindaZero v = oneNorm v < 1E-10

areKindaOrthogonal
  :: (Fractional (Scalar v), Ord (Scalar v), InnerSpace v) =>
     v -> v -> Bool
areKindaOrthogonal v0 v1 = abs (v0 <.> v1) < 1E-10 

oneNorm :: (Num c, Foldable t) => t c -> c
oneNorm = getSum . foldMap (Sum . abs)

stdToUnit1 :: Tup2 t -> t
stdToUnit1 (Tup2 (_,x1)) = x1
stdToUnit2 ::  Tup3 a -> Tup2 a
stdToUnit2 (Tup3 (_,x1,x2)) = Tup2 (x1,x2)
stdToUnit3 ::  Tup4 a -> Tup3 a
stdToUnit3 (Tup4 (_,x1,x2,x3)) = Tup3 (x1,x2,x3)

unitToStd1 :: Num a => a -> Tup2 a
unitToStd1 x1 = Tup2 (1-x1,x1)
unitToStd2 :: Num a => Tup2 a -> Tup3 a
unitToStd2 (Tup2 (x1,x2)) = Tup3 (1-x1-x2,x1,x2)
unitToStd3 ::  Num a => Tup3 a -> Tup4 a
unitToStd3 (Tup3 (x1,x2,x3)) = Tup4 (1-x1-x2-x3,x1,x2,x3)


std2IntoStd3 :: Num a => 
       Index4 -- ^ Index of zero component is 3 minus this  
    -> Tup3 a -> Tup4 a
std2IntoStd3 i (Tup3 (u,v,w)) =
                    case i of
                         I4_0 -> Tup4 (u,v,w,0)
                         I4_1 -> Tup4 (u,v,0,w)
                         I4_2 -> Tup4 (u,0,v,w)
                         I4_3 -> Tup4 (0,u,v,w)


stdBasisVector4 :: Num a => Index4 -> Tup4 a
stdBasisVector4 i = Tup4 (tupleFromFun4 (\j -> if i==j then 1 else 0))

combo2 :: VectorSpace v => ((Scalar v, v), (Scalar v, v)) -> v
combo2 ((x0,v0), (x1,v1)) =
     x0 *^ v0 
    ^+^
    x1 *^ v1 

combo3
  :: VectorSpace v =>
     ((Scalar v, v), (Scalar v, v), (Scalar v, v)) -> v
combo3 ((x0,v0), (x1,v1), (x2,v2)) =
     x0 *^ v0 
    ^+^
    x1 *^ v1 
    ^+^
    x2 *^ v2 



combo4
  :: VectorSpace v =>
     ((Scalar v, v), (Scalar v, v), (Scalar v, v), (Scalar v, v)) -> v
combo4 ((x0,v0), (x1,v1), (x2,v2), (x3,v3)) =
     x0 *^ v0 
    ^+^
    x1 *^ v1 
    ^+^
    x2 *^ v2 
    ^+^
    x3 *^ v3 

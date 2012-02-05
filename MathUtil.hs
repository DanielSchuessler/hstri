{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, StandaloneDeriving, TemplateHaskell, RecordWildCards, ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE TypeFamilies, FlexibleInstances, BangPatterns #-} 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module MathUtil where


import Control.Exception
import Control.Monad
import Data.Complex
import Data.Maybe
import Data.Ratio
import Data.Vect.Double hiding(Vector)
import qualified Data.Vect.Double as Vect
import System.Random
import Test.QuickCheck
import qualified Data.Foldable as Fold
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed(Unbox)
import Data.Vector(Vector)
import HomogenousTuples
import Control.Arrow((&&&))
import Util
import OrphanInstances()

anyOrth :: Vec3 -> Vec3
anyOrth (Vec3 0 y z) = Vec3 0 (-z) y
anyOrth (Vec3 x y _) = Vec3 (-y) x 0

stereograph :: Vec4 -> Vec3
stereograph (Vec4 x0 x1 x2 x3) = Vec3 x1 x2 x3 &* (recip (1+x0))

stereograph' :: Double -> Vec4 -> Vec3
stereograph' t (Vec4 y0 y1 y2 y3) = 
    let 
        Vec2 y0' y3' = rotMatrix2 t *. Vec2 y0 y3

    in stereograph (Vec4 y0' y1 y2 y3')

c2_to_r4 :: Complex Double -> Complex Double -> Vec4
c2_to_r4 ((:+) !a !b) ((:+) !c !d) = Vec4 a b c d 



arb01 :: (Num a, Random a) => Gen a
arb01 = frequency [(10,return 0),(30,choose (0,1)) ] 

arbsimp :: Gen Vec4
arbsimp = do
    xs <- liftM4join4 Vec4 arb01 `suchThat` (/= Vect.zero)
    return (xs &/ (xs `dotprod` 1))
    

(&/) :: Vect.Vector v => v -> Double -> v
v &/ r = v &* recip r
    

{-# INLINABLE to3Sphere #-}
to3Sphere ::  Vec4 -> Vec4
to3Sphere (Vec4 x0 x1 x2 x3) = c2_to_r4 g1 g2  
    where
        x01 = x0+x1
        x23 = x2+x3
        h = pi * (1+x01-x23) / 4
        g1 | x01 == 0 = 0
           | otherwise = sin h `cScalarMul` cis (2*pi*x0/x01) 

        g2 | x23 == 0 = 0
           | otherwise = cos h `cScalarMul` cis (2*pi*x2/x23) 

-- instance AbelianGroup (Complex Double) where
--     zero = 0
--     (&+) = (+)
--     (&-) = (-)
--     neg = negate
-- 
-- instance Vector (Complex Double) where
--     mapVec f (a :+ b) = f a :+ f b

cScalarMul :: Double -> Complex Double -> Complex Double
cScalarMul r (a :+ b) = r*a :+ r*b

withOrigin :: AbelianGroup b => b -> (b -> b) -> b -> b
withOrigin o f = (&+ o) . f . (&- o)



-- | 'vec3Z' * @pointZTo z'@ = @z'@ 
pointZTo :: Vec3 -> Mat3
pointZTo z' = Mat3 x' y' z'
    where
                x' = normalize (anyOrth z')
                y' = normalize (crossprod x' z')



-- prop_g1 = forAll arbsimp (\x -> let y = to3Sphere x
--                                 in abs (1-normsqr y) < 1E-14) 
                                
matrixApproxEq :: MatrixNorms m => m -> m -> Bool
matrixApproxEq m1 m2 = matrixDistance m1 m2 < 1E-10




                  
lcms :: (Integral b, Fold.Foldable t) => t b -> b
lcms = Fold.foldl' lcm 1

denomLcms :: (Integral b, Fold.Foldable t) => t (Ratio b) -> b
denomLcms = Fold.foldl' (\r x -> lcm (denominator x) r) 1


        
makeMatrixIntegral
  :: (Integral b, VG.Vector v (Ratio b), VG.Vector v b) =>
     Vector (v (Ratio b)) -> Vector (v b)
makeMatrixIntegral = V.map makeVecIntegral

makeVecIntegral
  :: (Integral b, VG.Vector v (Ratio b), VG.Vector v b) =>
     v (Ratio b) -> v b
makeVecIntegral ro =
    let
        l = VG.foldl' lcm 1 . VG.map denominator $ ro

        f = fromMaybe (assert False undefined)
                . ratioToIntegral
    in
        VG.map (f . (fromIntegral l*)) ro

-- instance Integral i => AdditiveGroup (Ratio i) where
--     zeroV = 0
--     (^+^) = (+)
--     negateV = negate








-- | Returns a tetrahedration of a three-sided prism, and the newly created edges and (inner triangles)
tetrahedrate3Prism
  :: (t, t, t)
     -> (t, t, t) -> ([(t, t, t, t)], [(t, t)], [(t, t, t)])
tetrahedrate3Prism (a,b,c) (a',b',c') =
    ( [ list4 a b c c', list4 a b b' c', list4 a a' b' c' ] 
    , [(a,c'),(b,c'),(a,b')]
    , [(a,b,c'),(a,b',c')])



incircleRadius :: Floating a => a -> a -> a -> a
incircleRadius a b c =
    let s = (a+b+c) / 2
    in sqrt ((s-a)*(s-b)*(s-c)/s)


type UnitIntervalPoint = Double

-- | Convex hull of @(0,0), (1,0), (0,1)@
type Unit2SimplexPoint = Vec2

-- | Convex hull of @(0,0), (1,0), (0,1), (1,1)@
type UnitSquare = Vec2

toUnitSquareNE :: Unit2SimplexPoint -> UnitSquare
toUnitSquareNE = (Vec2 1 1 &-)


-- | Triangulation of the triangle with corners @(0,0),(0,steps),(steps,0)@, using isosceles triangles with scele length 1
triangulateTriangle
  :: (Enum t, Num t) => t -> ([(t, t)], [((t, t), (t, t), (t, t))])
triangulateTriangle steps = (verts,ts)
    where
        m = steps-1

        verts = [(u,v) | u <- [0..m], v <- [0..m-u]]

        ts = 
                       [ ((u,v)
                         ,(u+1,v)
                         ,(u,v+1))
                         
                            | u <- [0..m-1], v <- [ 0..m-1-u ] ]  
                        ++
                       [ ((u,v)                             
                         ,(u,v+1)                           
                         ,(u-1,v+1))                          
                                                                
                            | u <- [1..m-1], v <- [ 0..m-1-u ] ]


bary2 :: Vect.Vector v => (v, v) -> v
bary2 (x, y) = (x &+ y) &/ 2

bary3 :: Vect.Vector v => (v, v, v) -> v
bary3 (x, y, z) = (x &+ y &+ z) &/ 3


hillClimb :: Ord b => (a -> [a]) -> (a -> b) -> a -> a
hillClimb successors badness initial = go (initial,badness initial)
    where
        go (x,curBadness) =
           case L.find ((<curBadness) . snd) .  map (id &&& badness) . successors $ x of
                Nothing -> x
                Just suc -> go suc

data SolidTorusPoint = STP {
    long :: Double, 
    lat :: Double, 
    -- | 0 to 1, 1 is on the boundary
    boundaryness :: Double 

}

mapSolidTorus
  :: Mat2 -> SolidTorusPoint -> SolidTorusPoint
mapSolidTorus (Mat2 (Vec2 a b) (Vec2 c d)) (STP long lat boundaryness) =
    STP (a*long + b*lat) (c*long + d*lat) boundaryness

torusBoundary :: UnitSquare -> SolidTorusPoint
torusBoundary (Vec2 long lat) = STP (2*pi*long) (2*pi*lat) 1

meridionalDisc :: UnitSquare -> SolidTorusPoint
meridionalDisc (Vec2 x y) = STP 0 (2*pi*x) y
-- meridionalDisc p0 = STP 0 (angle2 p) (2*max (abs x) (abs y))
--     where
--         p@(Vec2 x y) = p0 &- Vec2 0.5 0.5 

torusCoords :: 
       Double -- ^ Major radius
    -> Double -- ^ Minor radius
    -> SolidTorusPoint
    -> Vec3
torusCoords major minor (STP long lat boundaryness) =
    let
        minor' = minor * boundaryness 
        r = major + cos lat * minor' 
    in
        Vec3 (cos long * r) (sin long * r) (sin lat * minor') 
        

class NonNegScalable r a | a -> r where
    scaleNonNeg :: r -> a -> a 

instance Num r => NonNegScalable r (V.Vector r) where
    scaleNonNeg = V.map . (*)

instance (Unbox r, Num r) => NonNegScalable r (VU.Vector r) where
    scaleNonNeg = VU.map . (*)

ratioToIntegral_Ratio :: Integral a => Ratio a -> Maybe a
ratioToIntegral_Ratio r = guard (denominator r == 1) >> Just (numerator r)

class RatioToIntegral r i | r -> i where
    -- | Map a rational value which is actually an integral value to the underlying integral type. Return @Nothing@ if the input isn't an integer. 
    ratioToIntegral :: r -> Maybe i

instance Integral i => RatioToIntegral (Ratio i) i where
    ratioToIntegral = ratioToIntegral_Ratio

instance RatioToIntegral r i => RatioToIntegral (V.Vector r) (V.Vector i) where
    ratioToIntegral = V.mapM ratioToIntegral

instance (Unbox r, Unbox i, RatioToIntegral r i) => RatioToIntegral (VU.Vector r) (VU.Vector i) where
    ratioToIntegral = VU.mapM ratioToIntegral


-- | @codegeneracy k@ monotonically maps the integers to themselves such that every integer has exactly one preimage, except @k@, which has these two:
--
-- @codegeneracy k k = codegeneracy k (k+1) = k@.
codegeneracy :: (Num a, Ord a) => a -> a -> a
codegeneracy k i = 
    if i <= k
        then i
        else i-1

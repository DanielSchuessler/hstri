{-# LANGUAGE StandaloneDeriving, TemplateHaskell, RecordWildCards, ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE FlexibleInstances, BangPatterns #-} 
module MathUtil where

import Data.Vect.Double as Vect
import Data.Complex
import Test.QuickCheck
import Control.Monad
import TupleTH
import Data.Vect.Double.Instances
import Data.Vect.Double.Util.Dim4
import Control.Monad.LPMonad
import Data.Algebra
import Data.LinearProgram
import Control.Applicative
import Control.Exception
import System.Directory
import System.FilePath
import Test.QuickCheck.Gen
import System.Random
import System.Process
import System.Exit
import Data.Word

anyOrth :: Vec3 -> Vec3
anyOrth (Vec3 0 y z) = Vec3 0 (-z) y
anyOrth (Vec3 x y _) = Vec3 (-y) x 0

stereograph (Vec4 x0 x1 x2 x3) = Vec3 x1 x2 x3 &* (recip (1+x0))

stereograph' :: Double -> Vec4 -> Vec3
stereograph' t (Vec4 y0 y1 y2 y3) = 
    let 
        Vec2 y0' y3' = rotMatrix2 t *. Vec2 y0 y3

    in stereograph (Vec4 y0' y1 y2 y3')

c2_to_r4 :: Complex Double -> Complex Double -> Vec4
c2_to_r4 ((:+) !a !b) ((:+) !c !d) = Vec4 a b c d 

instance Arbitrary Vec4 where
    arbitrary = gen4 arbitrary

gen4 g = liftM4 Vec4 g g g g

arb01 = frequency [(10,return 0),(30,choose (0,1)) ] 

arbsimp :: Gen Vec4
arbsimp = do
    xs <- gen4 arb01 `suchThat` (/= Vect.zero)
    return (xs &/ (xs `dotprod` 1))
    

(&/) :: Vector v => v -> Double -> v
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

deriving instance Ord Vec3


-- | 'vec3Z' * @pointZTo z'@ = @z'@ 
pointZTo z' = Mat3 x' y' z'
    where
                x' = normalize (anyOrth z')
                y' = normalize (crossprod x' z')



-- prop_g1 = forAll arbsimp (\x -> let y = to3Sphere x
--                                 in abs (1-normsqr y) < 1E-14) 
                                

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module QuadCoordinates.Class
    (module INormalDisc,
     module Data.FormalOps,

     QuadCoords(..)
     )


    where

import INormalDisc
import Data.FormalOps
import Control.Applicative
import Control.Arrow
import Data.Monoid
import qualified Data.Map as M
import Data.Map(Map)

class Num r => QuadCoords q r | q -> r where
     quadCount :: q -> INormalQuad -> r
                                         
     -- | May (but need not) omit zero coefficients. May contain repeated quads.
     quadAssocs :: q -> [(INormalQuad,r)]
     quadAssocs = quadAssocsDistinct

     -- | May (but need not) omit zero coefficients.
     quadAssocsDistinct :: q -> [(INormalQuad,r)]
     quadAssocsDistinct = M.toList . M.fromListWith (+) . quadAssocs

instance QuadCoords INormalQuad Integer where
    quadCount q q' = if q==q' then 1 else 0
    quadAssocsDistinct q = [(q,1)]

instance QuadCoords INormalDisc Integer where
    quadCount = eitherIND quadCount quadCount
    quadAssocsDistinct = eitherIND quadAssocs quadAssocs

-- | Constant zero; only needed as a superclass
instance QuadCoords INormalTri Integer where
    quadCount = const (const 0)
    quadAssocsDistinct = const []

instance (Num n, QuadCoords q n) => QuadCoords [q] n where
    quadCount xs q' = sum (flip quadCount q' <$> xs) 
    quadAssocs = concatMap quadAssocs 

instance (Num n, QuadCoords q n) => QuadCoords (FormalProduct n q) n where
    quadCount (n :* q) = (n *) <$> quadCount q
    quadAssocs (n :* q) = second (n *) <$> quadAssocs q
    quadAssocsDistinct (n :* q) = second (n *) <$> quadAssocsDistinct q

instance (Num n, QuadCoords q n, QuadCoords q' n) => QuadCoords (FormalSum q q') n where
    quadCount = fmap evalFormalSum . bitraverseFormalSum quadCount quadCount
    quadAssocs = foldFormalSum (++) . bimapFormalSum quadAssocs quadAssocs



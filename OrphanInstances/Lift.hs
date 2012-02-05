{-# LANGUAGE TemplateHaskell, ViewPatterns, MagicHash #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module OrphanInstances.Lift where

import Data.Map(Map)
import GHC.Word
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import qualified Data.Map as M
import Data.Ratio
import Data.Vect.Double.Base

instance Lift Word8 where
    lift w = [| W8# $(litE (wordPrimL (fromIntegral w))) |] 

instance Lift Word where
    lift w = [| W# $(litE (wordPrimL (fromIntegral w))) |] 

instance Lift Double where
    lift x = [| d / n |] 
        where
            r = toRational x
            d = denominator r
            n = numerator r

-- instance Lift (BitSet a) where
--     lift (BitSet w) = [| BitSet w |]

instance (Ord k, Lift k, Lift v) => Lift (Map k v) where
    lift (M.toAscList -> al) = [| M.fromDistinctAscList al |]



deriveLiftMany [''Vec2,''Vec3,''Vec4]

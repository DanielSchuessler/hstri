{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, NoMonomorphismRestriction #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module ZapUnits where
import Control.Arrow
import Data.Proxy

class UnitZappingProd a b c where
    unitZappingProd :: (a,b) -> c

instance UnitZappingProd a b (a,b) 
    where unitZappingProd = id

instance (a~b) => UnitZappingProd a () b 
    where unitZappingProd = fst

instance (a~b) => UnitZappingProd () a b 
    where unitZappingProd = snd

instance UnitZappingProd () () () 
    where unitZappingProd = const ()

class ZapUnits a b where
    zapUnits :: a -> b


zapUnitsImpl :: forall a0 b0 a b c. (ZapUnits a0 a, ZapUnits b0 b, UnitZappingProd a b c) => 
    Proxy (a,b) -> (a0,b0) -> c
zapUnitsImpl _ = (unitZappingProd :: (a,b) -> c) . (zapUnits *** zapUnits)

instance (ZapUnits a0 a, ZapUnits b0 b, UnitZappingProd a b c) => ZapUnits (a0,b0) c where
    zapUnits = zapUnitsImpl (undefined :: Proxy (a,b)) 

instance ZapUnits a a where
    zapUnits = id

-- Overlap resolvers

instance ZapUnits (a0,b0) (a0,b0) where
    zapUnits = id

instance ZapUnits a b => ZapUnits (a,()) b where
    zapUnits = zapUnits . fst

instance ZapUnits a b => ZapUnits ((),a) b where
    zapUnits = zapUnits . snd

instance (b~()) => ZapUnits ((),()) b where
    zapUnits = const ()

instance ZapUnits ((),()) () where
    zapUnits = const ()

instance ZapUnits ((),()) ((),()) where
    zapUnits = id

-- 
-- instance ZapUnits (a,()) a where
--     zapUnits = fst

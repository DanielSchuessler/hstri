{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module CheckAdmissibility where

import Triangulation.Class
import Data.Proxy
import QuadCoordinates.Class
import QuadCoordinates.MatchingEquations
import Data.SumType
import Util
import StandardCoordinates.Class
import StandardCoordinates.MatchingEquations


-- | @adm q@ is the wrapper type for vectors admissible with respect to the normal surface coordinate system @coordsys@.
class AdmissibilityTyCon coordsys adm | coordsys -> adm, adm -> coordsys

instance AdmissibilityTyCon QuadCoordSys QAdmissible
instance AdmissibilityTyCon StdCoordSys Admissible

-- | 
--
--  [c] Coordinate system (phantom) type
--
--  [s] Coordinate vector type
--
--  [adm] Admissible coordinate vector type constructor for the coordinate system
--
--  [r] Coefficient type of both @s@ and @adm s@
class (NormalSurfaceCoefficients s r, NormalSurfaceCoefficients (adm s) r, AdmissibilityTyCon c adm) => CheckAdmissibility c s adm r where 
    admissible :: ToTriangulation tr => Proxy c -> tr -> s -> Either String (adm s)

instance (QuadCoords q r) => CheckAdmissibility QuadCoordSys q QAdmissible r where 
    admissible _ = quad_admissible

isAdmissible
  :: (ToTriangulation tr, CheckAdmissibility c s adm r) =>
     Proxy c -> tr -> s -> Bool
isAdmissible p = (.) isRight . admissible p

toAdmissible
  :: (Show s, ToTriangulation tr, CheckAdmissibility c s adm r) =>
     Proxy c -> tr -> s -> adm s
toAdmissible p tr x = either _err id . admissible p tr $ x
    where
        _err e = error ("toAdmissible "++showsPrec 11 x ""++": "++e)


instance (StandardCoords s r) => CheckAdmissibility StdCoordSys s Admissible r where
    admissible _ = standard_admissible

quad_isAdmissible
  :: (QuadCoords q r, ToTriangulation tr) => tr -> q -> Bool
quad_isAdmissible = isAdmissible quadCoordSys

standard_isAdmissible
  :: (ToTriangulation tr, StandardCoords s r) => tr -> s -> Bool
standard_isAdmissible = isAdmissible stdCoordSys


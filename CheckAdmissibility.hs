{-# LANGUAGE TemplateHaskell, FlexibleContexts, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
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


-- | 
--
--  [c] Coordinate system (phantom) type
--
--  [s] Coordinate vector type
--
--  [r] Coefficient type of both @s@ and @adm s@
class (NormalSurfaceCoefficients s r, NormalSurfaceCoefficients (AdmissibleFor c s) r) => CheckAdmissibility c s r where 

    admissible :: ToTriangulation tr => Proxy c -> tr -> s -> AttemptC (AdmissibleFor c s)

instance (Show r, QuadCoords q r, Show q) => CheckAdmissibility QuadCoordSys q r where 
    admissible _ = quad_admissible

isAdmissible
  :: (ToTriangulation tr, CheckAdmissibility c s r) =>
     Proxy c -> tr -> s -> Bool
isAdmissible p = (.) isRight . admissible p

toAdmissible
  :: (Show s, ToTriangulation tr, CheckAdmissibility c s r) =>
     Proxy c -> tr -> s -> AdmissibleFor c s
toAdmissible p tr x = $unEitherC _err . admissible p tr $ x
    where
        _err = "toAdmissible "++showsPrec 11 x ""


instance (Show r, StandardCoords s r, Show s) => CheckAdmissibility StdCoordSys s r where
    admissible _ = standard_admissible

quad_isAdmissible
  :: (Show r, QuadCoords q r, ToTriangulation tr, Show q) => tr -> q -> Bool
quad_isAdmissible = isAdmissible quadCoordSys

standard_isAdmissible
  :: (Show r, ToTriangulation tr, StandardCoords s r, Show s) => tr -> s -> Bool
standard_isAdmissible = isAdmissible stdCoordSys


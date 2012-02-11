{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module QuadCoordinates.Tests where

import Triangulation.AbstractNeighborhood
import Tetrahedron
import Control.Applicative
import Control.Arrow((&&&))
import Control.Monad.State
import Data.Foldable(Foldable)
import Data.Function
import Data.List as L
import Data.Map as M hiding(mapMaybe)
import Data.Maybe as May
import Data.SumType
import Data.VectorSpace
import Tetrahedron.INormalDisc
import PrettyUtil
import QuadCoordinates
import QuadCoordinates.Class
import QuadCoordinates.MatchingEquations
import QuickCheckUtil
import StandardCoordinates(StandardCoordinates,stc_toAssocs)
import StandardCoordinates.Class
import StandardCoordinates.MatchingEquations
import Test.QuickCheck
import Test.QuickCheck.All
import Triangulation.Random()
import Triangulation.VertexLink
import TriangulationCxtObject
import Math.SparseVector
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

prop_standardToQuad_VertexLinkingSurface :: Triangulation -> Property
prop_standardToQuad_VertexLinkingSurface (tr :: Triangulation) = 
    forAll (elements (vertices tr)) 
        (\v -> standardToQuad (vertexLinkingSurface v) == zeroV)

prop_quad_toFromDenseList :: Triangulation -> Property
prop_quad_toFromDenseList tr =
        forAll (unrestrictedQCGen tr) 
            (\(qc :: QuadCoordinates Int) -> 
                quad_fromDenseList tr (quad_toDenseList tr qc) .=. qc)

prop_satisfiesQMatchingEquationsControl
  :: Triangulation -> Property
prop_satisfiesQMatchingEquationsControl tr =
    expectFailure (forAll (unrestrictedQCGen tr
        :: Gen (QuadCoordinates Integer)) (isRight . satisfiesQMatchingEquations tr))



qc_QuadCoordinates = $quickCheckAll

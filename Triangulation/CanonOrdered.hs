{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
-- | Canonical vertex ordering for faces of a triangulation
module Triangulation.CanonOrdered(
    module Triangulation,
    CanonOrdered(..),COIEdge,COITriangle,
    CanonicallyOrderable(..),isCanonicallyOrdered,tCOIEdges,tCOITriangles,
    -- * Testing
    qc_CanonOrdered
) where



import AbstractTetrahedron
import Control.Applicative
import Control.Monad.Reader
import Data.Function
import Equivalence
import Prelude hiding(catch,lookup)
import Test.QuickCheck
import PrettyUtil
import FacetGluing
import QuickCheckUtil
import Data.Proxy
import Triangulation
import Test.QuickCheck.All



newtype CanonOrdered a = UnsafeCanonOrdered { unCanonOrdered :: a }
    deriving(Show,Eq,Ord,TriangulationDSnakeItem,Pretty )

type COITriangle = CanonOrdered OITriangle
type COIEdge = CanonOrdered OIEdge

class OrderableFace t ot => CanonicallyOrderable t ot where
    orderCanonically :: Triangulation -> t -> CanonOrdered ot
    allCanonicallyOrdered :: Triangulation -> [CanonOrdered ot] 

isCanonicallyOrdered
  :: (Eq ot, CanonicallyOrderable t ot) => Triangulation -> ot -> Bool
isCanonicallyOrdered tr x =
    x == (unCanonOrdered . orderCanonically tr . forgetVertexOrder) x


tCOITriangles :: Triangulation -> [CanonOrdered OITriangle]
tCOITriangles tr = UnsafeCanonOrdered <$> do
        tri <- tITriangles tr
        case lookupGluingOfITriangle tr tri of
            Nothing -> [ toOrderedFace tri ]
            Just otri
                | tri < forgetVertexOrder otri -> [ toOrderedFace tri, otri ]
                | otherwise -> []

-- | = 'tCOITriangles'
instance CanonicallyOrderable ITriangle OITriangle where
    allCanonicallyOrdered = tCOITriangles 

    orderCanonically tr tri = UnsafeCanonOrdered $
        case lookupGluingOfITriangle tr tri of
            Nothing -> toOrderedFace tri
            Just otri
                | tri < forgetVertexOrder otri -> toOrderedFace tri
                | otherwise -> snd (flipGluing (tri,otri))


tCOIEdges
  :: Triangulation -> [COIEdge]
tCOIEdges tr = UnsafeCanonOrdered <$> do
        cl <- eqvClasses (oEdgeEqv tr)
        guard (isCanonicallyOrderedClass cl) 
        ec_elementList cl       
            
instance CanonicallyOrderable IEdge OIEdge where
    allCanonicallyOrdered = tCOIEdges 

    orderCanonically tr e = UnsafeCanonOrdered $
        let
            oe = toOrderedFace e
        in
            oe *. (getVertexOrder (eqvRep (oEdgeEqv tr) oe))


--polyprop_CanonicallyOrderable :: CanonicallyOrderable t ot => Proxy ot -> Triangulation -> Property
polyprop_CanonicallyOrderable
  :: (Eq ot, Show ot, CanonicallyOrderable t ot) =>
     Proxy ot -> Triangulation -> Property
polyprop_CanonicallyOrderable (_ :: Proxy ot) tr =
    forAllElements (allCanonicallyOrdered tr :: [CanonOrdered ot])
        (isCanonicallyOrdered tr . unCanonOrdered)

--     .&.
-- 
--     forAllElements 


prop_CanonicallyOrderable_Triangle :: Triangulation -> Property
prop_CanonicallyOrderable_Triangle = polyprop_CanonicallyOrderable (undefined :: Proxy OITriangle)

prop_CanonicallyOrderable_Edge :: Triangulation -> Property
prop_CanonicallyOrderable_Edge = polyprop_CanonicallyOrderable (undefined :: Proxy OIEdge)

qc_CanonOrdered :: IO Bool
qc_CanonOrdered = $quickCheckAll
{-# LANGUAGE FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, MultiParamTypeClasses #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Vertex 
    (
    module FaceClasses,
    module TIndex,
    -- * Vertices
    Vertex,
    vA,vB,vC,vD,
    allVertices,allVertices',
    MakeVertex(..),
    Vertices(..),
    IVertex,
    vertexTripleToString,
    abstractTetrahedronColor,
    vertexToWord8,
    vertexFromWord8,

    Link(..),
    Star(..)


    )

    where

import Collections
import Control.Exception
import Data.BitSet.Word8
import Data.Word
import Element
import Language.Haskell.TH.Syntax as Syntax
import Test.QuickCheck
import PrettyUtil
import Util
import TIndex
import FaceClasses
import HomogenousTuples
import Control.Applicative

-- | Vertex of an abstract tetrahedron
newtype Vertex = Vertex Word8 deriving(Eq,Ord)

instance BitSetable Vertex

instance Bounded Vertex where
    minBound = vA
    maxBound = vD

instance Show Vertex where
    show (Vertex v) = case v of
                           0 -> "A"
                           1 -> "B"
                           2 -> "C"
                           3 -> "D"
                           _ -> error ("Tried to show bad Vertex with tag "++show v)

instance Enum Vertex where
    fromEnum (Vertex v) = fromEnum v
    toEnum n = assert (n >= 0 && n < 4) (Vertex (toEnum n))

class MakeVertex a where
    vertex :: a -> Vertex

vA,vB,vC,vD :: Vertex
vA = Vertex 0
vB = Vertex 1
vC = Vertex 2
vD = Vertex 3

allVertices ::  [Vertex]
allVertices = asList allVertices' 

allVertices' ::  (Vertex, Vertex, Vertex, Vertex)
allVertices' = (vA,vB,vC,vD)


vertexPrettyColor :: Doc -> Doc
vertexPrettyColor = cyan

instance Pretty Vertex where pretty = vertexPrettyColor . text . show

instance Arbitrary Vertex where arbitrary = elements allVertices

instance Lift Vertex where
    lift (Vertex v) = [| Vertex $(Syntax.lift v) |]


instance Finite Vertex


 
instance Quote Vertex where
    quotePrec _ v = "v" ++ show v

deriveCollectionKeyClass ''Vertex


vertexTripleToString :: (Vertex,Vertex,Vertex) -> String
vertexTripleToString (u,v,w) = concatMap show [u,v,w]


-- | A 'Vertex' with a tetrahedron index attached to it
data IVertex = IVertex {-# UNPACK #-} !TIndex {-# UNPACK #-} !Vertex
    deriving(Eq,Ord)

instance HasTIndex IVertex Vertex where
    viewI (IVertex i x) = I i x
    (./) = IVertex

instance Enum IVertex where
    toEnum (toEnum -> I i x) = (./) i x
    fromEnum = fromEnum . viewI

instance Show IVertex where show = show . viewI
instance Quote IVertex where quotePrec prec = quotePrec prec . viewI
instance Pretty IVertex where pretty = pretty . viewI

abstractTetrahedronColor ::  Doc -> Doc
abstractTetrahedronColor = cyan

instance Vertices TIndex (Quadruple IVertex) where
    vertices z = map4 (z ./) allVertices'

instance Arbitrary IVertex where
    arbitrary = (./) <$> arbitrary <*> arbitrary

instance Vertices AbsTet (Quadruple Vertex) where
    vertices = const allVertices'

vertexToWord8 :: Vertex -> Word8
vertexToWord8 (Vertex w) = w

vertexFromWord8 :: Word8 -> Vertex
vertexFromWord8 = Vertex

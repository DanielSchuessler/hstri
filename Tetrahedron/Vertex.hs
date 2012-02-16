{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, TypeFamilies, DeriveGeneric, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, MultiParamTypeClasses #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Tetrahedron.Vertex 
    (
    module FaceClasses,
    module TIndex,

    Vertex(..),
    vA,vB,vC,vD,
    allVertices,allVertices',
    MakeVertex(..),
    Vertices(..),
    IVertex,
    vertexTripleToString,
    abstractTetrahedronColor,
    vertexToWord8,
    vertexFromWord8,
    vertexDefaultCoords,
    vertexMemo,
    otherVertices,
    otherIVerticesInSameTet,
    vertexNu,
    index4ToVertex,
    vertexToIndex4,

    Link(..),
    Star(..),

    -- * View
    VertexView,
    viewVertex,unviewVertex,



    )

    where

import Control.Applicative
import Data.Binary
import Data.Binary.Derive
import Data.Maybe
import Data.Numbering
import Data.Vect.Double(Vec3(..),vec3X,vec3Y,vec3Z)
import Data.Vect.Double.Base((&-))
import DisjointUnion
import Element
import FaceClasses
import GHC.Generics hiding(prec)
import HomogenousTuples
import PrettyUtil
import Quote
import ShortShow
import TIndex
import Test.QuickCheck
import Util
import Data.Ix
import FileLocation
import Data.Tuple.Index
import Control.DeepSeq.TH
import Language.Haskell.TH.Lift
import Data.Typeable

data Vertex = A | B | C | D
    deriving(Eq,Ord,Bounded,Ix,Typeable)

-- | Must remain in @allVertices'@-compatible order!!
deriving instance Enum Vertex


type VertexView = Vertex

vA,vB,vC,vD :: Vertex
vA = A
vB = B
vC = C
vD = D

viewVertex :: Vertex -> VertexView
viewVertex = id

vertexFromWord8 :: Word8 -> Vertex
vertexFromWord8 w = case w of
                           0 -> A
                           1 -> B
                           2 -> C
                           3 -> D
                           _ -> error ("vertexFromWord8: Invalid tag: "++show w)

vertexToWord8 :: Vertex -> Word8
vertexToWord8 v = case v of
                       A -> 0
                       B -> 1
                       C -> 2
                       D -> 3


unviewVertex :: VertexView -> Vertex
unviewVertex = id



instance Show Vertex where
    show v = case viewVertex v of
                           A -> "A"
                           B -> "B"
                           C -> "C"
                           D -> "D"

class MakeVertex a where
    vertex :: a -> Vertex


allVertices ::  [Vertex]
allVertices = asList allVertices' 

-- | Must remain in @Enum Vertex@-compatible order!!
allVertices' ::  (Vertex, Vertex, Vertex, Vertex)
allVertices' = (vA,vB,vC,vD)


vertexPrettyColor :: Doc -> Doc
vertexPrettyColor = cyan

instance Pretty Vertex where pretty = vertexPrettyColor . text . show

instance Arbitrary Vertex where arbitrary = elements allVertices



instance Finite Vertex


 
instance Quote Vertex where
    quotePrec _ v = "v" ++ show v



vertexTripleToString :: (Vertex,Vertex,Vertex) -> String
vertexTripleToString (u,v,w) = concatMap show [u,v,w]


-- | A 'Vertex' with a tetrahedron index attached to it
data IVertex = IVertex {-# UNPACK #-} !TIndex {- UNPACK -} !Vertex
    deriving(Eq,Ord,Generic,Ix,Typeable)

instance Binary IVertex where
    put = derivePut
    get = deriveGet

instance HasTIndex IVertex Vertex where
    viewI (IVertex i x) = I i x
    (./) = IVertex

mapTIndicesFromHasTIndex [t|IVertex|]

instance Enum IVertex where
    toEnum (toEnum -> I i x) = (./) i x
    fromEnum = fromEnum . viewI

instance Show IVertex where show = show . viewI
instance ShortShow IVertex where shortShow = shortShow . viewI
instance Quote IVertex where quotePrec prec = quotePrec prec . viewI
instance Pretty IVertex where pretty = pretty . viewI

abstractTetrahedronColor ::  Doc -> Doc
abstractTetrahedronColor = cyan

instance Vertices TIndex where
    type Verts TIndex = Quadruple IVertex
    vertices z = map4 (z ./) allVertices'

instance Arbitrary IVertex where
    arbitrary = (./) <$> arbitrary <*> arbitrary


-- | Embeds the abstract tetrahedron into R^3 symmetrically
vertexDefaultCoords :: Vertex -> Vec3
vertexDefaultCoords = (\x -> f x &- center_) . viewVertex 
    where
        f A = vec3X
        f B = vec3Y
        f C = vec3Z
        f D = Vec3 1 1 1

        center_ = Vec3 0.5 0.5 0.5





instance ShortShow Vertex where shortShow = show

-- | Memoize a vertex-consuming function
vertexMemo :: (Vertex -> r) -> Vertex -> r
vertexMemo f = 
    let
        fA = f vA
        fB = f vB
        fC = f vC
        fD = f vD
    in
        \v -> case viewVertex v of
                   A -> fA
                   B -> fB
                   C -> fC
                   D -> fD


-- | Other vertices, ascending
otherVertices :: Vertex -> (Triple Vertex)
otherVertices = fromJust . flip deleteTuple4 allVertices'


instance Binary Vertex where
    put = put . vertexToWord8
    get = vertexFromWord8 <$> get


isRegardedAsSimplexByDisjointUnionDeriving ''DIM0 [t|Vertex|]
isRegardedAsSimplexByDisjointUnionDeriving ''DIM0 [t|IVertex|]

otherIVerticesInSameTet :: IVertex -> Triple IVertex
otherIVerticesInSameTet = traverseI map3 otherVertices

vertexNu :: Numbering Vertex
vertexNu = finiteTypeNu
                    

index4ToVertex :: Index4 -> Vertex
index4ToVertex = tupleToFun4 allVertices'

vertexToIndex4 :: Vertex -> Index4
vertexToIndex4 = $(fromJst) . indexOf4 allVertices'

deriveNFData ''Vertex
deriveNFData ''IVertex
deriveLift ''Vertex

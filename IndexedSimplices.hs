{-# LANGUAGE NoMonomorphismRestriction, CPP, TemplateHaskell, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module IndexedSimplices(
    module Data.Word,
    module NormalDisc,
    module ONormal,
    TIndex, tindex,
    I(..),
    HasTIndex(..),getTIndex,forgetTIndex,
    IVertex,IEdge,OIEdge,ITriangle,OITriangle,OINormalArc,
    INormalArc,
    MakeINormalArc(..),
    INormalCorner,
    INormalDisc
    
    ) where


import AbstractTetrahedron
import Collections
import Control.Applicative
import Control.Monad.Writer
import Data.Word
import HomogenousTuples
import NormalDisc
import ONormal
import Prelude hiding(catch,lookup)
import Test.QuickCheck hiding((.&.))
import Text.PrettyPrint.ANSI.Leijen hiding((<$>))
                    


-- | Tetrahedron index
newtype TIndex = TIndex Word
    deriving(Eq,Ord,Pretty,Enum)

tindex ::  Word -> TIndex
tindex = TIndex

-- | NOTE: Only fromInteger is supported (arithmetic doesn't make much sense on these)
instance Num TIndex where
    fromInteger = tindex . fromIntegral
    (+) = error ("(+) not supported for TIndex")
    (*) = error ("(+) not supported for TIndex")
    abs = error ("abs not supported for TIndex")
    signum = error ("signum not supported for TIndex")

-- | Thing with a tetrahedron index attached to it
data I a = I TIndex a 
    deriving(Eq,Ord)


-- | Instances of this class essentially say that @ia@ is isomorphic to @('TIndex',a)@ (but the representation is left open to for optimization)
class HasTIndex ia a | ia -> a where
    -- | Unpack some tetrahedron-indexed entity
    viewI :: ia -> I a
    -- | Attach a tetrahedron index to some entity
    (./) :: TIndex -> a -> ia

infix 9 ./

instance HasTIndex TIndex () where
    viewI = flip I () 
    (./) = const

getTIndex ::  HasTIndex a a' => a -> TIndex
getTIndex (viewI -> I i _) = i

forgetTIndex ::  HasTIndex a a' => a -> a'
forgetTIndex (viewI -> I _ a) = a 

instance HasTIndex (I a) a where
    viewI = id
    (./) = I

instance Show TIndex where
    show (TIndex i) = show i


-- | A 'Vertex' with a tetrahedron index attached to it
data IVertex = IVertex {-# UNPACK #-} !TIndex {-# UNPACK #-} !Vertex
    deriving(Eq,Ord)

instance Enum IVertex where
    toEnum (toEnum -> I i x) = (./) i x
    fromEnum = fromEnum . viewI

-- | An 'Edge' with a tetrahedron index attached to it
data IEdge = IEdge {-# UNPACK #-} !TIndex {-# UNPACK #-} !Edge
    deriving (Eq,Ord)

instance Enum IEdge where
    toEnum (toEnum -> I i x) = (./) i x
    fromEnum = fromEnum . viewI

-- | A 'Triangle' with a tetrahedron index attached to it
data ITriangle = ITriangle {-# UNPACK #-} !TIndex !Triangle
    deriving (Eq,Ord)

instance Enum ITriangle where
    toEnum (toEnum -> I i x) = (./) i x
    fromEnum = fromEnum . viewI

-- | An 'OEdge' with a tetrahedron index attached to it
type OIEdge = I OEdge 

-- | An 'OTriangle' with a tetrahedron index attached to it
type OITriangle = I OTriangle


instance HasTIndex IVertex Vertex where
    viewI (IVertex i x) = I i x
    (./) = IVertex


--     viewI (IVertex j) = I (shiftR j 2) (toEnum (fromIntegral (j .&. 3)))
--     (./) i v = IVertex (shiftL i 2 .|. fromIntegral (fromEnum v))

instance HasTIndex IEdge Edge where
    viewI (IEdge i x) = I i x
    (./) = IEdge
--     viewI (IEdge j) = I (shiftR j 3) (toEnum (fromIntegral (j .&. 7)))
--     (./) i v = IEdge (shiftL i 3 .|. fromIntegral (fromEnum v))

instance HasTIndex ITriangle Triangle where
    viewI (ITriangle i x) = I i x
    (./) = ITriangle

instance Vertices IEdge (Pair IVertex) where 
    {-# INLINABLE vertices #-}
    vertices z = map2 ((./) (getTIndex z)) (vertices (forgetTIndex z)) 

instance Vertices OIEdge (Pair IVertex) where 
    vertices z = map2 ((./) (getTIndex z)) (vertices (forgetTIndex z)) 

instance Vertices ITriangle (Triple IVertex) where 
    vertices z = map3 ((./) (getTIndex z)) (vertices (forgetTIndex z)) 

instance Vertices OITriangle (Triple IVertex) where 
    vertices z = map3 ((./) (getTIndex z)) (vertices (forgetTIndex z)) 



instance (Show a) => Show (I a) where 
    showsPrec prec (I i x) = showParen (prec >= 1) (showsPrec 10 i . showString " ./ " . showsPrec 10 x)

instance Quote TIndex where
    quotePrec _ (TIndex i) = show i -- relies on Num TIndex instance

instance (Quote a) => Quote (I a) where 
    quotePrec prec (I i x) = quoteParen (prec >= 1) (quotePrec 10 i ++ " ./ " ++ quotePrec 10 x)
    

instance Show IVertex where show = show . viewI
instance Show IEdge where show = show . viewI
instance Show ITriangle where show =  show . viewI

instance Quote IVertex where quotePrec prec = quotePrec prec . viewI
instance Quote IEdge where quotePrec prec = quotePrec prec . viewI
instance Quote ITriangle where quotePrec prec =  quotePrec prec . viewI


instance (Pretty a) => Pretty (I a) where
    pretty (I x y) = dullcyan (pretty x) <> dot <> pretty y

instance Pretty IVertex where pretty = pretty . viewI
instance Pretty IEdge where pretty = pretty . viewI
instance Pretty ITriangle where pretty = pretty . viewI




instance (FaceType IEdge OIEdge) where
    type VertexSymGroup IEdge = VertexSymGroup Edge
    type VertexTuple IEdge = Pair IVertex
    unpackOrderedFace (viewI -> I i (unpackOrderedFace -> (g,e))) = (g, (./) i e)
    packOrderedFace g (viewI -> I i e) = (./) i (packOrderedFace g e)

instance  (FaceType ITriangle OITriangle) where
    type VertexSymGroup ITriangle = VertexSymGroup Triangle
    type VertexTuple ITriangle = Triple IVertex
    unpackOrderedFace (viewI -> I i (unpackOrderedFace -> (g,t))) = (g, (./) i t)
    packOrderedFace g (viewI -> I i t) = (./) i (packOrderedFace g t)




instance LeftAction S2 OIEdge where (.*) = defaultLeftActionForOrderedFace
instance LeftAction S3 OITriangle where (.*) = defaultLeftActionForOrderedFace





instance Edges ITriangle (Triple IEdge) where
    edges (viewI -> I i t) = map3 (i ./) (edges t)

instance Edges OITriangle (Triple OIEdge) where
    edges (viewI -> I i t) = map3 (i ./) (edges t)





-- instance Show TIndex where
--     show (TIndex x) = "\916"++show x

instance Vertices TIndex (Quadruple IVertex) where
    vertices z = map4 (z ./) allVertices'

instance Edges TIndex (IEdge,IEdge,IEdge,IEdge,IEdge,IEdge) where
    edges z = map6 (z ./) allEdges'

instance Triangles TIndex (Quadruple ITriangle) where
    triangles z = map4 (z ./) allTriangles'




instance (Arbitrary a) => Arbitrary (I a) where
    arbitrary = I <$> arbitrary <*> arbitrary

instance Arbitrary TIndex where
    arbitrary = tindex `fmap` (changeSize (`div` 5) arbitrary :: Gen Word)


type INormalArc = I NormalArc
type INormalCorner = I NormalCorner
type INormalDisc = I NormalDisc
type OINormalArc = I ONormalArc

instance (IsSubface a b) => IsSubface (I a) (I b) where
    isSubface (I i a) (I j b) = i == j && isSubface a b

instance IsSubface IVertex IEdge where
    isSubface x y = isSubface (viewI x) (viewI y)


instance HasNormalCorners INormalArc (Pair INormalCorner) where
    normalCorners (viewI -> I i a) = map2 (i ./) (normalCorners a) 

instance Edges IVertex (Triple IEdge) where
    edges (viewI -> I i v) = map3 (i ./) (edges v) 

instance Triangles IVertex (Triple ITriangle) where
    triangles (viewI -> I i v) = map3 (i ./) (triangles v) 

instance Triangles IEdge (Pair ITriangle) where
    triangles (viewI -> I i v) = map2 (i ./) (triangles v) 

instance Finite a => Enum (I a) where
    toEnum n = case toEnum n of EnumPair a b -> I a b
    fromEnum (I a b) = fromEnum (EnumPair a b)

#define F(T) instance IsSubface T TIndex where isSubface x i = getTIndex x == i 
F(IVertex)
F(IEdge)
F(ITriangle)
#undef F

-- $(concat `liftM` 
--     mapM  (\t -> 
--         [d| 
--             instance IsSubface $(t) TIndex where isSubface x i = $(varE '(==)) (getTIndex x) i 
--           |])
--     [conT ''IVertex, conT ''IEdge, conT ''ITriangle] 
--  )


class MakeINormalArc a where
    iNormalArc :: a -> INormalArc

instance MakeINormalArc (ITriangle,Vertex) where
    iNormalArc (viewI -> I i t,v) = (./) i (normalArc (t, v))

instance MakeINormalArc (OITriangle,Vertex) where
    iNormalArc (t,v) = iNormalArc (forgetVertexOrder t,v)

instance MakeINormalArc (Triangle,IVertex) where
    iNormalArc (t,viewI -> I i v) = (./) i (normalArc (t, v))
    

deriveCollectionKeyClass ''IVertex
deriveCollectionKeyClass ''IEdge
deriveCollectionKeyClass ''ITriangle


#ifdef USE_TRIEMAP
instance Repr TIndex where
    type Rep TIndex = Rep Word
    type RepList TIndex = DRepList TIndex

    toRep (TIndex w) = toRep w
    toRepList = dToRepList

instance (Repr a) => Repr (I a) where
    type Rep (I a) = (Rep Int, Rep a)
    type RepList (I a) = Vector (Rep Int, Rep a)

    toRep (I x y) = (toRep x,toRep y)
    toRepList = dToRepList
#endif

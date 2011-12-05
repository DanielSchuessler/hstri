{-# LANGUAGE TypeSynonymInstances, BangPatterns, DeriveFoldable, DeriveTraversable, DeriveFunctor, ViewPatterns, TypeFamilies, NoMonomorphismRestriction, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, FunctionalDependencies, FlexibleContexts, FlexibleInstances, CPP #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module Util where
import Collections
import Control.DeepSeq
import Control.Monad
import Data.Bits
import Data.Colour.SRGB as SRGB(RGB(..),toSRGB,Colour) 
import Data.Function
import Data.Graph.Inductive.Graph(labEdges)
import Data.Graph.Inductive.Graph(labNodes)
import Data.Graph.Inductive.PatriciaTree(Gr)
import Data.Int
import Data.List as List
import Data.Monoid
import Data.Tagged
import Data.Word
import Language.Haskell.TH
import Test.QuickCheck hiding((.&.))
import Test.QuickCheck.All
import Data.BitSet.Word8(BitSet(..))
import Language.Haskell.TH.Syntax
import Data.Char
import qualified Data.Set as S
import Control.Exception


cart ::  Monad m => m a1 -> m a2 -> m (a1, a2)
cart = liftM2 (,)

(.*.) ::  Monoid a => a -> a -> a
(.*.) = mappend
infixr 7 .*.

polyprop_idl ::  (Monoid a, Eq a) => a -> Bool
polyprop_idl g = mempty .*. g == g

polyprop_idr ::  (Monoid a, Eq a) => a -> Bool
polyprop_idr g = g .*. mempty == g

polyprop_assoc ::  (Monoid a, Eq a) => a -> a -> a -> Bool
polyprop_assoc g3 g2 g1 = (g3 .*. g2) .*. g1 == g3 .*. (g2 .*. g1)


class Monoid g => Group g where
    inv :: g -> g


polyprop_invl ::  (Group a, Eq a) => a -> Bool
polyprop_invl g = inv g .*. g == mempty

polyprop_invr ::  (Group a, Eq a) => a -> Bool
polyprop_invr g = g .*. inv g == mempty

class LeftAction g x where
    (.*) ::  g -> x -> x

infixr 7 .*

-- We need a type-level tag here because the monoid isn't inferrable from the acted-upon type
polyprop_act_id :: forall a x. (Monoid a, LeftAction a x, Eq x) => Tagged a (x -> Bool)
polyprop_act_id = Tagged (\x -> (mempty :: a) .* x == x)


polyprop_act_mult :: (LeftAction g x, Monoid g, Eq x) => g -> g -> x -> Bool
polyprop_act_mult g2 g1 x = (g2 .*. g1) .* x == g2 .* g1 .* x



forAllElements :: (Testable prop, Show a) => [a] -> (a -> prop) -> Property
forAllElements [] _ = label "Vacuously true (empty domain)" True
forAllElements xs p = forAll (elements xs) p

forAllElements2 :: (Testable prop, Show a1, Show a2) =>[a1] -> [a2] -> ((a1, a2) -> prop) -> Property
forAllElements2 xs ys p = forAllElements (xs `cart` ys) p



forAll2 :: (Testable prop, Show a1, Show a) =>Gen a -> Gen a1 -> (a -> a1 -> prop) -> Property
forAll2 gen1 gen2 p = forAll gen1 (\x1 -> forAll gen2 (p x1))
forAll3 :: (Testable prop, Show a1, Show a2, Show a) =>Gen a -> Gen a1 -> Gen a2 -> (a -> a1 -> a2 -> prop) -> Property
forAll3 gen1 gen2 gen3 p = forAll gen1 (\x1 -> forAll gen2 (\x2 -> forAll gen3 (p x1 x2)))





nub' ::  Ord a => [a] -> [a]
nub' = S.toList . S.fromList


changeSize ::  (Int -> Int) -> Gen a -> Gen a
changeSize f gen = sized (\n -> resize (f n) gen) 


isRight ::  Either t t1 -> Bool
isRight (Right _) = True
isRight _ = False

fromRight ::  Show a => Either a c -> c
fromRight = either (error . f) id
    where
        f x = "fromRight (Left "++showsPrec 11 x ""++")" 





{-# SPECIALIZE indexInTriple :: (Show t, Eq t) => t -> (t,t,t)-> Int #-}
-- | Returns the first index if more than one element of the triple is equal to the element. Errors if none is.
indexInTriple ::  (Show t, Num a, Eq t) => t -> (t, t, t) -> a
indexInTriple x (x1,x2,x3) | x == x1 = 0
                           | x == x2 = 1
                           | x == x3 = 2
                           | otherwise = error ( unwords [ "indexInTriple" , show x, show (x1,x2,x3) ] )




class (Enum a, Bounded a) => Finite a where

-- | Like 'Either', but I use a newtype to avoid orphan instances
data EnumEither a b = EnumLeft a | EnumRight a
    deriving(Eq,Show)

data EnumPair a b = EnumPair a b
     deriving(Eq,Ord,Show)   

toEither ::  EnumEither b t -> Either b b
toEither (EnumLeft a) = Left a
toEither (EnumRight a) = Right a

toEitherEnum ::  Either a a -> EnumEither a b
toEitherEnum (Left a) = EnumLeft a
toEitherEnum (Right a) = EnumRight a


either' ::  (b -> c) -> (b -> c) -> EnumEither b t -> c
either' l r = either l r . toEither


-- | Like 'toEnum', but shifted to return the smallest element at zero.
toEnum0 :: forall a. (Enum a, Bounded a) => Int -> a
toEnum0 n = toEnum (n + minBoundIndex (undefined :: a))

-- | Like 'fromEnum', but shifted to return zero for the smallest element.
fromEnum0 :: forall a. (Enum a, Bounded a) => a -> Int
fromEnum0 x = fromEnum x - minBoundIndex (undefined :: a)

-- | Does not evaluate the argument.
minBoundIndex :: forall a. (Enum a, Bounded a) => a -> Int
minBoundIndex = const (fromEnum (minBound :: a))

-- | Does not evaluate the argument.
maxBoundIndex :: forall a. (Enum a, Bounded a) => a -> Int
maxBoundIndex = const (fromEnum (maxBound :: a))


-- instance (Enum a, Finite b) => Enum (EnumEither a b) where
--     fromEnum (EnumLeft a) = 2 * fromEnum a
--     fromEnum (EnumRight b) = 2 * fromEnum b + 1
-- 
--     toEnum ((`divMod` 2) -> (d,m)) = case m of
--                                         0 -> EnumLeft (toEnum d)
--                                         1 -> EnumRight (toEnum d)
-- 
-- 
-- 
-- instance (Bounded a, Bounded b) => Bounded (EnumEither a b) where
--     minBound = EnumLeft minBound
--     maxBound = EnumRight maxBound
-- 
-- instance (Finite a, Finite b) => Finite (EnumEither a b)

instance (Enum a, Finite b) => Enum (EnumPair a b) where
    fromEnum = (\(EnumPair a b) -> fromEnum a * card_b + fromEnum0 b)
        where
            card_b = cardinality (undefined :: b)

    toEnum = (\n -> let (d,m) = divMod n card_b in EnumPair (toEnum d) (toEnum0 m)) 
        where
            card_b = cardinality (undefined :: b)

instance (Bounded a, Bounded b) => Bounded (EnumPair a b) where
    minBound = EnumPair minBound minBound
    maxBound = EnumPair maxBound maxBound

instance (Finite a, Finite b) => Finite (EnumPair a b)

instance Finite Bool
instance Finite ()

instance (Arbitrary a, Arbitrary b) => Arbitrary (EnumEither a b) where arbitrary = toEitherEnum `fmap` arbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (EnumPair a b) where arbitrary = EnumPair `fmap` arbitrary `ap` arbitrary

-- prop_EnumEither ::  EnumEither Int16 Char -> Bool
-- prop_EnumEither x = toEnum (fromEnum x) == x 


prop_EnumPair ::  EnumPair Int16 Bool -> Bool
prop_EnumPair x = toEnum (fromEnum x) == x 

    
-- | Returns the cardinality of the whole /type/ @a@. The argument is just a dummy and is not evaluated.
cardinality :: forall a. Finite a => a -> Int
cardinality _ = maxBoundIndex (undefined :: a) - minBoundIndex (undefined :: a) + 1

qc_Util ::  IO Bool
qc_Util = $(quickCheckAll)        


-- | Exhaustively checks a property for all elements of a list (in contrast to 'forAll', which samples randomly)
conjoinMap ::  Testable prop => [t] -> (t -> prop) -> Property
conjoinMap [] _ = label "Vacuously true (empty domain)" True
conjoinMap xs p = conjoin (fmap p xs)

conjoinMap2 :: Testable prop => [a1] -> [a2] -> ((a1, a2) -> prop) -> Property
conjoinMap2 xs ys p = conjoinMap (xs `cart` ys) p




(.=.) ::  (Show a, Eq a) => a -> a -> Property
x .=. y = 
    printTestCase (unlines ["Equality failed:","=== FIRST ELEMENT ===",show x,"=== SECOND ELEMENT ===",show y])
    (x==y)


showBits :: forall a. Bits a => a -> String
showBits = (\x -> fmap (\i -> if testBit x i then '1' else '0') [n-1,n-2..0]) 
    where
        n = bitSize (undefined :: a)



concatMapM ::  Monad m => (z -> m [a]) -> [z] -> m [a]
concatMapM f = liftM concat . mapM f


instance (NFData n, NFData e) => NFData (Gr n e) where
    rnf g = rnf (labNodes g, labEdges g)

instance (NFData (Colour Double)) where
    rnf c = case toSRGB c of 
                 SRGB.RGB r g b -> rnf (r,g,b)









word8ToNibbles ::  Word8 -> (Word8,Word8)
word8ToNibbles !x = (shiftR x 4, x .&. 15) 

nibblesToWord8 :: (Word8,Word8) -> Word8
nibblesToWord8 (!x1,!x0) = shiftL x1 4 .|. x0


flipNibbles :: Word8 -> Word8
flipNibbles !x = rotateL x 4

prop_word8ToNibbles ::  Word8 -> Property
prop_word8ToNibbles x = x .=. nibblesToWord8 (word8ToNibbles_noInline x)

{-# NOINLINE word8ToNibbles_noInline #-}
word8ToNibbles_noInline ::  Word8 -> (Word8, Word8)
word8ToNibbles_noInline = word8ToNibbles

{-# RULES "word8ToNibblesToWord8" forall x. nibblesToWord8 (word8ToNibbles x) = x #-}
{-# RULES "flipNibbles^2" forall x. flipNibbles (flipNibbles x) = x #-}

prop_flipNibbles ::  Word8 -> Bool
prop_flipNibbles x = word8ToNibbles (flipNibbles x) == (y0,y1)
    where
        (y1,y0) = word8ToNibbles x


        

        

deriveCollectionKeyClass ''BitSet

instance Lift Word8 where
    lift w = sigE (litE (IntegerL (toInteger w))) [t| Word8 |]

instance Lift (BitSet a) where
    lift (BitSet w) = [| BitSet $(lift w) |]


-- | Things that can be outputed as valid haskell source code
class Quote a where
    quotePrec :: Int -> a -> String

quote ::  Quote a => a -> String
quote = quotePrec 0

quoteParen ::  Bool -> [Char] -> [Char]
quoteParen True x = "(" ++ x ++ ")"
quoteParen False x = x

instance Quote a => Quote [a] where
    quotePrec _ xs = "[" ++ intercalate ", " (fmap quote xs) ++ "]"

instance (Quote a, Quote b) => Quote (a,b) where
    quotePrec _ (x,y) = quoteParen True (quote x ++ ", "++quote y)


-- charsToPermute ::  Int -> [Char] -> Permute
-- charsToPermute n = listPermute n . fmap f
--     where
--         f c = ord c - ord 'a'
-- 
-- 
-- permuteToChars ::  Permute -> [Char]
-- permuteToChars = fmap f . Permute.elems 
--     where
--         f i = chr (i + ord 'a')


-- | Suitable for types with only nullable constructors and derived Show instance
liftByShow ::  Show a => a -> ExpQ
liftByShow = conE . mkName . show



wrapException :: String -> a -> a
wrapException msg = mapException (\(SomeException e) -> 
    ErrorCall (unlines[msg,"Inner Exception:",show e]))



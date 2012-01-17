{-# LANGUAGE TupleSections, TypeSynonymInstances, BangPatterns, DeriveFoldable, DeriveTraversable, DeriveFunctor, ViewPatterns, TypeFamilies, NoMonomorphismRestriction, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, FunctionalDependencies, FlexibleContexts, FlexibleInstances, CPP #-}
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
import Data.Word
import Test.QuickCheck hiding((.&.))
import Test.QuickCheck.All
import Data.BitSet.Word8(BitSet(..))
import Control.Exception
import QuickCheckUtil
import Data.Set as S
import Data.Functor
import Data.Char
import System.Process
import System.Exit
import Data.Time.Clock.POSIX
import Data.Maybe
import Data.Binary(getWord8)
import Data.Binary(putWord8)
import Data.Binary(Get)
import Data.Binary(Put)
import Data.List.Split
import Control.Applicative
import Safe






fi :: (Integral a, Num b) => a -> b
fi = fromIntegral


nub' ::  Ord a => [a] -> [a]
nub' = S.toList . S.fromList




isRight ::  Either t t1 -> Bool
isRight (Right _) = True
isRight _ = False

fromRight ::  Show a => Either a c -> c
fromRight = either (error . f) id
    where
        f x = "fromRight (Left "++showsPrec 11 x ""++")" 








class (Enum a, Bounded a) => Finite a where

-- | Like 'Either', but I use a new type to avoid orphan instances
data EnumEither a b = EnumLeft a | EnumRight a
    deriving(Eq,Show)

data EnumPair a b = EnumPair a b
     deriving(Eq,Ord,Show)   

toEither ::  EnumEither b t -> Either b b
toEither (EnumLeft a) = Left a
toEither (EnumRight a) = Right a

toEnumEither ::  Either a a -> EnumEither a b
toEnumEither (Left a) = EnumLeft a
toEnumEither (Right a) = EnumRight a


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

instance (Arbitrary a, Arbitrary b) => Arbitrary (EnumEither a b) where arbitrary = toEnumEither `fmap` arbitrary
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



showBits :: forall a. Bits a => a -> String
showBits = (\x -> fmap (\i -> if testBit x i then '1' else '0') [n-1,n-2..0]) 
    where
        n = bitSize (undefined :: a)



concatMapM ::  Monad m => (z -> m [a]) -> [z] -> m [a]
concatMapM f = liftM concat . mapM f

mapMaybeM :: Monad m => (a1 -> m (Maybe a)) -> [a1] -> m [a]
mapMaybeM f = liftM catMaybes . mapM f


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





wrapException :: String -> a -> a
wrapException msg = mapException (\(SomeException e) -> 
    ErrorCall (unlines[msg,"Inner Exception:",show e]))



        
        
uncurry3 :: (t1 -> t2 -> t3 -> t) -> (t1, t2, t3) -> t
uncurry3 f (a,b,c) = f a b c



subscriptify :: String -> String
subscriptify = List.map (chr . (+n) . ord)
    where
        n = 8320 - ord '0'

        
showSubscript :: (Show a, Integral a) => a -> String
showSubscript = subscriptify . show 

rawSystemS :: String -> [String] -> IO ()
rawSystemS p args = do
    putStrLn (unwords (p:fmap q args))
    ec <- rawSystem p args
    when (ec /= ExitSuccess)
        (error (show (p,args) ++ " -> "++show ec))

 where
    q x = "'" ++ concatMap (\c -> if c == '\'' then "''" else [c]) x ++ "'"

systemS :: String -> IO ()
systemS l = do
    putStrLn l
    ec <- system l
    when (ec /= ExitSuccess)
        (error (show l ++ " -> "++show ec))


 


extend :: Monad m => (a -> m b) -> m a -> m b
extend = (=<<)

atLeastTwo :: Bool -> Bool -> Bool -> Bool
atLeastTwo a b c = 
    (if a then (||) else (&&)) b c

tmpfn :: String -> IO FilePath
tmpfn stem = (("/tmp/" ++ stem) ++) . show . fromEnum <$> getPOSIXTime

getEnumWord8 :: Enum b => Get b
getEnumWord8 = (toEnum . fromIntegral) <$> getWord8

putEnumWord8 :: Enum a => a -> Put
putEnumWord8 = putWord8 . fromIntegral . fromEnum 


findJust :: (a1 -> Maybe a) -> [a1] -> Maybe a
findJust f = foldr (\x r -> case f x of
                        Nothing -> r
                        j@(Just _) -> j)
                    Nothing

orElse :: Maybe c -> c -> c
orElse = flip fromMaybe
infixr 4 `orElse`


parseFloatLiterals :: String -> [Double]
parseFloatLiterals =
    mapMaybe readMay .
    wordsBy (not <$> ((||) <$> isDigit <*> (`elem` "-+.Ee")))

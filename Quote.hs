{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Quote where
import Data.List
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Proxy

class QuoteConstPat a where
    quoteConstPat :: a -> String
    quoteConstPat_view :: Proxy a -> String -> String
    quoteConstPat_view = const id

-- | Things that can be output as valid haskell source code
class Quote a where
    quotePrec :: Int -> a -> String
    quotePrec _ = quote

    quote ::  Quote a => a -> String
    quote = quotePrec 0

quoteParen ::  Bool -> [Char] -> [Char]
quoteParen True x = "(" ++ x ++ ")"
quoteParen False x = x

instance Quote a => Quote [a] where
    quotePrec _ xs = "[" ++ intercalate ", " (fmap quote xs) ++ "]"

instance (Quote a, Quote b) => Quote (a,b) where
    quotePrec _ (x,y) = quoteParen True (quote x ++ ", "++quote y)

instance (Quote a, Quote b, Quote c) => Quote (a,b,c) where
    quotePrec _ (x,y,z) = 
        quoteParen True 
            (quote x ++ ", "++quote y ++ ", "++quote z)

instance (Quote a, Quote b, Quote c, Quote d) => Quote (a,b,c,d) where
    quotePrec _ (x,y,z,a) = 
        quoteParen True 
            (quote x ++ ", "++quote y ++ ", "++quote z++", "++quote a)

instance Quote Word where quote = show

instance Quote String where quote = quoteStr 
                            
quoteStr :: String -> String                            
quoteStr = show

quoteApp :: Quote a => [Char] -> a -> [Char]
quoteApp x y = x ++ " (" ++ quotePrec 11 y ++ ")"

instance Quote BL.ByteString where
    quotePrec prec x = 
        quoteParen (prec > 10)
            ("Data.ByteString.Lazy.Char8.pack "++quoteStr (BL8.unpack x)) 

instance Quote BS.ByteString where
    quotePrec prec x = 
        quoteParen (prec > 10) $
            "Data.ByteString.Char8.pack " ++ (quoteStr (BS8.unpack x)) 




quoteFunWithDom
  :: forall a a1. (Quote a1, QuoteConstPat a) => [a] -> (a -> a1) -> String
quoteFunWithDom dom f = 
    unlines 
        (("\\x -> case "++quoteConstPat_view (undefined :: Proxy a) "x"++ " of") :
         [ "    "++quoteConstPat x++" -> "++quote (f x) 

            | x <- dom ])

        
quoteFun
  :: (Bounded a, Enum a, Quote a1, QuoteConstPat a) =>
     (a -> a1) -> String
quoteFun = quoteFunWithDom [minBound..maxBound]

quoteConstPatPair
  :: (QuoteConstPat a, QuoteConstPat a1) => (a, a1) -> [Char]
quoteConstPatPair (x,y) = show "("++quoteConstPat x++", "++quoteConstPat y++")"

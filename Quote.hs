{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Quote where
import Data.List
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

-- | Things that can be outputed as valid haskell source code
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

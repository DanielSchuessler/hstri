{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Quote where
import Data.List
import Data.Word

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

instance Quote String where quote = show

quoteApp :: Quote a => [Char] -> a -> [Char]
quoteApp x y = x ++ " (" ++ quotePrec 11 y ++ ")"

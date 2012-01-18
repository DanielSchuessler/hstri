{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module ShortShow where
import TypeLevel.TF.List
import Element
import Data.List
import Math.Groups.S2
import Math.Groups.S3
import Data.Word

-- | This class is for stringifying things (for humans) in contexts where characters are at a premium, e.g. Blender object names
class ShortShow a where
    shortShowsPrec :: Int -> a -> ShowS 
    shortShow :: a -> String

    shortShow = ($"") . shortShowsPrec 0
    shortShowsPrec _ x = (shortShow x ++) 


shortShows :: ShortShow a => a -> ShowS
shortShows = shortShowsPrec 0

instance (ShortShow a, ShortShow b) => ShortShow (a,b) where
    shortShowsPrec _ (a,b) = showChar '(' 
                        . shortShows a 
                        . showChar ',' . shortShows b
                        . showChar ')'

instance (ShortShow a, ShortShow b, ShortShow c) => ShortShow (a,b,c) where
    shortShowsPrec _ (a,b,c) = showChar '(' 
                        . shortShows a 
                        . showChar ',' . shortShows b
                        . showChar ',' . shortShows c
                        . showChar ')'

instance (ShortShow a, ShortShow b, ShortShow c, ShortShow d) => ShortShow (a,b,c,d) where
    shortShowsPrec _ (a,b,c,d) = showChar '(' 
                        . shortShows a 
                        . showChar ',' . shortShows b
                        . showChar ',' . shortShows c
                        . showChar ',' . shortShows d
                        . showChar ')'

instance ShortShow v => ShortShow (List v n) where
    shortShow = error "ShortShow (List v n): not implemented"


shortShowAsSet :: (AsList a, ShortShow (Element a)) => a -> [Char]
shortShowAsSet xs = "{" ++ (intercalate "," . fmap shortShow . asList) xs ++ "}"

instance ShortShow S2 where shortShow = show
instance ShortShow S3 where shortShow = show
instance ShortShow Int where shortShow = show
instance ShortShow Word where shortShow = show
instance ShortShow Integer where shortShow = show

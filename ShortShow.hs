{-# LANGUAGE TemplateHaskell, OverlappingInstances, FlexibleContexts, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module ShortShow where
import Element
import Data.List
import Math.Groups.S2
import Math.Groups.S3
import Data.Word
import THUtil

-- | This class is for stringifying things (for humans) in contexts where characters are at a premium, e.g. Blender object names. It's generally only intended to differentiate the @a@ from other @a@s 
-- occuring in the same "context" (e.g. Blender scene), not to uniquely determine it.
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

-- instance ShortShow v => ShortShow (List v n) where
--     shortShow = error "ShortShow (List v n): not implemented"


shortShowAsSet :: (AsList a, ShortShow (Element a)) => a -> [Char]
shortShowAsSet xs = "{" ++ (intercalate "," . fmap shortShow . asList) xs ++ "}"

instance ShortShow S2 where shortShow = show
instance ShortShow S3 where shortShow = show
instance ShortShow Int where shortShow = show
instance ShortShow Word where shortShow = show
instance ShortShow Integer where shortShow = show

instance ShortShow Bool where shortShow b = [if b then 'T' else 'F'] 

-- | Prints no indication of wheter the value is a Left or a Right, as it's usually clear. Write an OverlappingInstance otherwise.
instance (ShortShow a, ShortShow b) => ShortShow (Either a b) where shortShow = either shortShow shortShow

inheritShortShow
  :: (Convertible accessor ExpQ,
      Convertible sub TypeQ,
      Convertible super TypeQ) =>
     sub -> super -> accessor -> Q [Dec]
inheritShortShow = inheritSingleArgClass ''ShortShow ['shortShow] []


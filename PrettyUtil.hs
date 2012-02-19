{-# LANGUAGE StandaloneDeriving, TypeFamilies, ViewPatterns, NoMonomorphismRestriction, TemplateHaskell, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
-- | Description: Pretty-printing utilities
module PrettyUtil(
    -- * Reexports
    module Text.PrettyPrint.ANSI.Leijen,
    Semigroup.Semigroup,(Semigroup.<>),
    appPrec,appPrec1,
    -- * Main
    spacedEncloseSep,
    prettyListAsSet,
    prettyAssocs,
    prettyString,
    pr,
    prettyRecord,
    prettyShowsPrec,
    prettyFunction,
    prettyRealFloat,
    prettyPrecApp,
    parensIf,
    anyPretty,AnyPretty,
    prettyEqs,
    docToString,
    hencloseSep,
    htupled,
    pad,
    prettyPrecFromShow,
    -- * Class
    Pretty(..),
    
    ) 

    where

import Control.Arrow((***),(&&&))
import Data.Map(Map)
import Data.Ratio
import Data.Set(Set)
import Data.Vect.Double.Base
import Data.Word
import Element
import GHC.Show
import HomogenousTuples
import Numeric
import Text.PrettyPrint.ANSI.Leijen hiding((<$>),Pretty(..),(<>))
import TupleTH
import qualified Data.Map as M
import qualified Data.Semigroup as Semigroup
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Text.PrettyPrint.ANSI.Leijen as Leijen
import Text.Groom
import Numeric.AD.Types(AD)
import Data.FiniteFunc
--import GHC.Generics hiding(prec)

(<>) ::  Doc -> Doc -> Doc
(<>) = (Leijen.<>)

spacedEncloseSep ::  Doc -> Doc -> Doc -> [Doc] -> Doc
spacedEncloseSep l r separator = encloseSep (l <> space) (space <> r) (separator <> space)


prettyListAsSet :: (AsList xs, Pretty (Element xs)) => xs -> Doc
prettyListAsSet xs0 = case asList xs0 of
                          [] ->  char '∅' 
                          (x:xs) ->
                                align (
                                    fillCat 
                                        ((lbrace<>space<>pretty x)
                                        :
                                        map (((comma<>space)<>) . pretty) xs)
                                        </> rbrace
                                    )



prettyAssocs ::  (Pretty a1, Pretty a) => [(a, a1)] -> Doc
prettyAssocs = docAssocs . fmap (pretty *** pretty)

docAssocs :: [(Doc, Doc)] -> Doc
docAssocs xs =  spacedEncloseSep lbrace rbrace comma 
                    [ hang 2 (k </> string "->" </> v)
                        | (k,v) <- xs ]

prettyString ::  Pretty a => a -> String
prettyString = ($"") . docToShowS . pretty

pr ::  Pretty a => a -> IO ()
pr = putStrLn . prettyString
                                   



prettyRecord :: String -> [(String, Doc)] -> Doc
prettyRecord name fields =
         vsep ( (text name <+> lbrace)
               : [indent 2 (text x <+> nest 2 (char '=' </> y)) | (x,y) <- fields ]
               ++ [rbrace] )


prettyShowsPrec :: Pretty a => Int -> a -> ShowS
prettyShowsPrec prec x = docToShowS (prettyPrec prec x)

docToShowS :: Doc -> ShowS
docToShowS = displayS . renderPretty 1 116

docToString :: Doc -> String
docToString = ($"") . docToShowS


prettyFunction
  :: (AsList xs, Pretty y, Pretty (Element xs)) =>
     (Element xs -> y) -> xs -> Doc
prettyFunction f domain = prettyAssocs (fmap (id &&& f) (asList domain))

instance (Pretty a, Pretty b) => Pretty (FiniteFunc a b) where
    pretty ff = prettyFunction (ff_apply ff) (ff_domain ff)


prettyRealFloat :: RealFloat a => a -> Doc
prettyRealFloat = text . addSpace . ($"") . showFFloat (Just 3)
    where
        addSpace s = if head s == '-' then s else ' ':s
        
class Pretty a where
    prettyPrec :: Int -> a -> Doc
    pretty :: a -> Doc
    prettyPrecList    :: Int -> [a] -> Doc

    pretty = prettyPrec 0
    prettyPrec _ = pretty
    prettyPrecList _ = list . map pretty

-- | Note: use 'anyPretty' (not 'pretty') if the arg list is heterogenous
prettyPrecApp :: (Pretty f, Pretty bs) => Int -> f -> [bs] -> Doc
prettyPrecApp prec f xs =
    parensIf (prec > appPrec)
    (hang 2 $ sep 
        (prettyPrec appPrec f : fmap (prettyPrec appPrec1) xs))

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

-- | Like 'tupled', but with space after comma
tupled' = encloseSep lparen   rparen  (comma <> space)

instance (Pretty a,Pretty b) => Pretty (a,b) where
  pretty = tupled' . toList2 . $(mapTuple' 2 [| pretty |])

instance (Pretty a,Pretty b,Pretty c) => Pretty (a,b,c) where
  pretty = tupled' . toList3 . $(mapTuple' 3 [| pretty |])

instance (Pretty a,Pretty b,Pretty c,Pretty d ) => Pretty (a,b,c,d) where
  pretty = tupled' . toList4 . $(mapTuple' 4 [| pretty |])




instance Pretty Integer where pretty = integer
instance Pretty Word where pretty = pretty . toInteger
instance Pretty () where pretty () = text "()"
instance Pretty Bool where pretty b = bool b
instance Pretty Int where pretty i = int i

instance (Ord a, Pretty a) => Pretty (Set a) where
    pretty = prettyListAsSet . S.toList 

instance (Ord a, Pretty a, Pretty b) => Pretty (Map a b) where
    pretty = prettyAssocs . M.assocs 

instance Pretty Doc where pretty = id

instance Pretty a => Pretty [a] where
  prettyPrec    = prettyPrecList

instance Pretty Char where
  pretty c            = char c
  prettyPrecList _ s  = string s

instance Pretty a => Pretty (Maybe a) where
    prettyPrec _ Nothing = text "Nothing" 
    prettyPrec prec (Just a) = prettyPrecApp prec "Just" [a] 

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    prettyPrec prec (Left a)  = prettyPrecApp prec "Left" [a]
    prettyPrec prec (Right b) = prettyPrecApp prec "Right" [b]

instance Pretty Double where pretty = prettyRealFloat
instance Pretty Float where pretty = prettyRealFloat

instance Pretty Vec2 where
    pretty (Vec2 x y) = parens (hsep (toList2 $ map2 pretty (x,y)))

instance Pretty Vec3 where
    pretty (Vec3 x y z) = parens (hsep (toList3 $ map3 pretty (x,y,z)))


newtype AnyPretty = AnyPretty (Int -> Doc)

anyPretty :: Pretty a => a -> AnyPretty
anyPretty = AnyPretty . flip prettyPrec

instance Pretty AnyPretty where
    prettyPrec prec (AnyPretty prettyPrec_) = prettyPrec_ prec

-- instance Pretty v => Pretty (List v n) where
--     pretty = tupled . fmap pretty . lToList


prettyEqs :: [(String, Doc)] -> Doc
prettyEqs xs = lbrace <+> align bod <+> rbrace
    where
        bod = fillSep (punctuate (comma <> char ' ') ys)
        ys = fmap (\(xl,xr) -> align (pretty xl <+> char '=' <+> align (pretty xr))) xs


-- class GPretty f where
--     gpp :: Int -> f p -> Doc 
-- 
-- instance GPretty U1 where
--     gpp _ _ = empty
-- 
-- instance Pretty c => GPretty (K1 i c) where
--     gpp prec (K1 c) = prettyPrec prec c 
-- 
-- 
--

instance Pretty a => Pretty (V.Vector a) where
    pretty x = char 'V' <> align (pretty . asList $ x) 

instance (VU.Unbox a, Pretty a) => Pretty (VU.Vector a) where
    pretty x = char 'V' <> align (pretty . asList $ x) 


instance (Eq a, Integral a, Pretty a) => Pretty (Ratio a) where
    pretty 0 = char '0'
    pretty x = 
        let 
            n = pretty (numerator x)
            d = denominator x 
        in
            if d==1 
               then n
               else n <> char '/' <> pretty d


-- | Like 'encloseSep' but single-line
hencloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
hencloseSep l r sep_ xs =
    l <> hcat (punctuate sep_ xs) <> r

-- | Like 'tupled' but single-line
htupled :: [Doc] -> Doc
htupled = hencloseSep lbrace rbrace (text ", ")

prettyPrecFromShow :: Show a => Int -> a -> Doc
prettyPrecFromShow prec x = string . groomString $ showsPrec prec x ""

instance Pretty Word8 where pretty = text . show

instance (Show (AD f a)) => Pretty (AD f a) where
    prettyPrec = prettyPrecFromShow


pad :: Int -> [Char] -> [Char]
pad l x = replicate (l - length x) ' ' ++ x


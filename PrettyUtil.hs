{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module PrettyUtil(
    -- * Reexports
    module Text.PrettyPrint.ANSI.Leijen,
    appPrec,appPrec1,
    -- * Main
    spacedEncloseSep,
    prettyListAsSet,
    prettyAssocs,
    prettyString,
    pr,
    prettyStringMatrix,
    prettyMatrix,
    prMatrix,
    prettyRecord,
    prettyShowsPrec,
    prettyFunction,
    prettyRealFloat,
    prettyPrecApp,
    parensIf,
    anyPretty,AnyPretty,
    prettyEqs,
    docToString,
    -- * Class
    Pretty(..)
    
    ) 

    where

import Text.PrettyPrint.ANSI.Leijen hiding((<$>),Pretty(..))
import Data.Word
import Data.Ratio
import Data.List
import Collections
import qualified Data.Map as M
import Numeric
import Control.Arrow((***),(&&&))
import Element
import Data.Vect.Double.Base
import HomogenousTuples
import GHC.Show
import TupleTH
import TypeLevel.TF.List(List,lToList)
--import GHC.Generics hiding(prec)

spacedEncloseSep ::  Doc -> Doc -> Doc -> [Doc] -> Doc
spacedEncloseSep l r separator = encloseSep (l <> space) (space <> r) (separator <> space)


prettyListAsSet ::  Pretty a => [a] -> Doc
prettyListAsSet = spacedEncloseSep lbrace rbrace comma . fmap pretty



prettyAssocs ::  (Pretty a1, Pretty a) => [(a, a1)] -> Doc
prettyAssocs = docAssocs . fmap (pretty *** pretty)

docAssocs :: [(Doc, Doc)] -> Doc
docAssocs xs =  spacedEncloseSep lbrace rbrace comma 
                    [ hang 2 (k </> string "->" </> v)
                        | (k,v) <- xs ]

prettyString ::  Pretty a => a -> String
prettyString = ($"") . displayS . renderPretty 0.5 116 . pretty

pr ::  Pretty a => a -> IO ()
pr = putStrLn . prettyString
                                   

prettyStringMatrix :: [[String]] -> String
prettyStringMatrix xss = unlines . fmap (intercalate "  " . fmap fmtCell) $ xss 
    where
        maxWidth = maximum . fmap length . concat $ xss
        fmtCell x = replicate (maxWidth - length x) ' ' ++ x

class PrettyScalar a where
    prettyScalar :: a -> String

instance PrettyScalar Rational where
    prettyScalar 0 = ""
    prettyScalar x = show (numerator x) ++ "/" ++ show (denominator x)

instance PrettyScalar Double where
    prettyScalar 0 = ""
    prettyScalar x = showFFloat (Just 4) x "" 

prettyMatrix :: PrettyScalar a => [[a]] -> String
prettyMatrix = prettyStringMatrix . (fmap . fmap) prettyScalar

prMatrix :: PrettyScalar a => [[a]] -> IO ()
prMatrix = putStrLn . prettyMatrix 



prettyRecord :: String -> [(String, Doc)] -> Doc
prettyRecord name fields =
         vsep ( (text name <+> lbrace)
               : [indent 2 (text x <+> nest 2 (char '=' </> y)) | (x,y) <- fields ]
               ++ [rbrace] )


prettyShowsPrec :: Pretty a => Int -> a -> ShowS
prettyShowsPrec prec x = docToShowS (prettyPrec prec x)

docToShowS :: Doc -> ShowS
docToShowS = displayS . renderPretty 0.4 116

docToString :: Doc -> String
docToString = ($"") . docToShowS


prettyFunction
  :: (AsList xs, Pretty y, Pretty (Element xs)) =>
     (Element xs -> y) -> xs -> Doc
prettyFunction f domain = prettyAssocs (fmap (id &&& f) (asList domain))


prettyRealFloat :: RealFloat a => a -> Doc
prettyRealFloat = text . addSpace . ($"") . showFFloat (Just 3)
    where
        addSpace s = if head s == '-' then s else ' ':s
        
class Pretty a where
    prettyPrec :: Int -> a -> Doc
    pretty :: a -> Doc
    prettyList    :: [a] -> Doc

    pretty = prettyPrec 0
    prettyPrec _ = pretty
    prettyList    = list . map pretty

-- | Note: use 'anyPretty' (not 'pretty') if the arg list is heterogenous
prettyPrecApp :: (Pretty f, Pretty bs) => Int -> f -> [bs] -> Doc
prettyPrecApp prec f xs =
    parensIf (prec > appPrec)
    (hang 2 $ sep 
        (prettyPrec appPrec f : fmap (prettyPrec appPrec1) xs))

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id


instance (Pretty a,Pretty b) => Pretty (a,b) where
  pretty = tupled . toList2 . $(mapTuple' 2 [| pretty |])

instance (Pretty a,Pretty b,Pretty c) => Pretty (a,b,c) where
  pretty = tupled . toList3 . $(mapTuple' 3 [| pretty |])

instance (Pretty a,Pretty b,Pretty c,Pretty d ) => Pretty (a,b,c,d) where
  pretty = tupled . toList4 . $(mapTuple' 4 [| pretty |])



instance Pretty Integer where pretty = integer
instance Pretty Word where pretty = pretty . toInteger
instance Pretty () where pretty () = text "()"
instance Pretty Bool where pretty b = bool b
instance Pretty Int where pretty i = int i

instance (Integral a, Show a) => Pretty (Ratio a) where
    pretty = text . show

instance (Ord a, Pretty a) => Pretty (Set a) where
    pretty = prettyListAsSet . setToList 

instance (Ord a, Pretty a, Pretty b) => Pretty (Map a b) where
    pretty = prettyAssocs . M.assocs 

instance Pretty Doc where pretty = id

instance Pretty a => Pretty [a] where
  pretty        = prettyList

instance Pretty Char where
  pretty c      = char c
  prettyList s  = string s

instance Pretty a => Pretty (Maybe a) where
    prettyPrec _ Nothing = text "Nothing" 
    prettyPrec prec (Just a) = prettyPrecApp prec "Just" [a] 

instance (Pretty a, Pretty b) => Pretty (Either a b) where
    prettyPrec prec (Left a) = prettyPrecApp prec "Left" [a]
    prettyPrec prec (Right b) = prettyPrecApp prec "Right" [b]

instance Pretty Double where pretty = prettyRealFloat
instance Pretty Float where pretty = prettyRealFloat

instance Pretty Vec3 where
    pretty (Vec3 x y z) = parens (hsep (toList3 $ map3 pretty (x,y,z)))


newtype AnyPretty = AnyPretty (Int -> Doc)

anyPretty :: Pretty a => a -> AnyPretty
anyPretty = AnyPretty . flip prettyPrec

instance Pretty AnyPretty where
    prettyPrec prec (AnyPretty prettyPrec_) = prettyPrec_ prec

instance Pretty v => Pretty (List v n) where
    pretty = tupled . fmap pretty . lToList


prettyEqs :: (Pretty a, Pretty a1) => [(a, a1)] -> Doc
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

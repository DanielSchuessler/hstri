{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module PrettyUtil(
    module Text.PrettyPrint.ANSI.Leijen,
    spacedEncloseSep,
    prettyListAsSet,
    prettyAssocs,
    docAssocs,
    prettyString,
    pr,
    prettyStringMatrix,
    prettyMatrix,
    prMatrix,
    prettyRecord,
    prettyShowsPrec
    
    ) 

    where

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))
import Data.Word
import Data.Ratio
import Data.List
import Collections
import qualified Data.Map as M
import Numeric
import Control.Arrow((***))


spacedEncloseSep ::  Doc -> Doc -> Doc -> [Doc] -> Doc
spacedEncloseSep l r separator = encloseSep (l <> space) (space <> r) (separator <> space)


prettyListAsSet ::  Pretty a => [a] -> Doc
prettyListAsSet = spacedEncloseSep lbrace rbrace comma . fmap pretty



prettyAssocs ::  (Pretty a1, Pretty a) => [(a, a1)] -> Doc
prettyAssocs = docAssocs . fmap (pretty *** pretty)

docAssocs :: [(Doc, Doc)] -> Doc
docAssocs xs =  spacedEncloseSep lbrace rbrace comma 
                    [ k </> nest 2 (string "->" </> v)
                        | (k,v) <- xs ]

prettyString ::  Pretty a => a -> String
prettyString = ($"") . displayS . renderPretty 0.5 116 . pretty

pr ::  Pretty a => a -> IO ()
pr = putStrLn . prettyString
                                   
instance Pretty Word where pretty = pretty . toInteger

instance (Integral a, Show a) => Pretty (Ratio a) where
    pretty = text . show

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


instance (Ord a, Pretty a) => Pretty (Set a) where
    pretty = prettyListAsSet . setToList 

instance (Ord a, Pretty a, Pretty b) => Pretty (Map a b) where
    pretty = prettyAssocs . M.assocs 


prettyRecord :: String -> [(String, Doc)] -> Doc
prettyRecord name fields =
         vsep ( (text name <+> lbrace)
               : [indent 2 (text x <+> nest 2 (char '=' </> y)) | (x,y) <- fields ]
               ++ [rbrace] )


prettyShowsPrec :: Pretty a => Int -> a -> ShowS
prettyShowsPrec _ = displayS . renderPretty 0.4 116 . pretty

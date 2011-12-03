{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances, EmptyDataDecls, GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving, ExistentialQuantification, NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS -Wall #-} 
module PolymakeInterface where

import System.Directory
import System.Random
import qualified System.FilePath as File
import System.Process
import PrettyUtil
import Text.PrettyPrint.ANSI.Leijen as PP

import Control.Monad.Writer
import Data.List(intersperse)
import Text.Parsec as P
import Data.Ratio
import Data.Functor.Identity

type Parser a = ParsecT String () Identity a

pPmRational :: Parser Rational 
pPmRational = do
    spaces
    sgn <- option "" (P.string "-")
    digits <- many1 P.digit
    denom <- option "1" $ do
        spaces
        void (P.char '/')
        spaces
        many1 P.digit

    return ((read (sgn++digits) :: Integer) % (read denom :: Integer))

pPmFloat :: Parser Double
pPmFloat = do
    spaces
    it <- many1 (P.digit <|> oneOf "-+.")
    return (read it :: Double)

class RealFrac s => PmScalar s where
    pmScalarTypeName :: s -> String
    pmScalarParse :: Parser s
    pmScalarShow :: s -> String

instance PmScalar Rational where
    pmScalarTypeName _ = "Rational"
    pmScalarParse = pPmRational
    pmScalarShow x = show (numerator x) ++ " / " ++ show (denominator x)

instance PmScalar Double where
    pmScalarTypeName _ = "Float"
    pmScalarParse = pPmFloat
    pmScalarShow = show

data PmPolytope s

data PmExpr a where
    PmMatrix :: PmScalar s => [[PmExpr s]] -> PmExpr [[s]]
    PmPolytope :: PmScalar s => { 
                                 pmInequalities :: PmExpr [[s]]
                               , pmEquations :: PmExpr [[s]]
                               } -> PmExpr (PmPolytope s)


    PmGet :: PmExpr a -> PmProperty -> PmExpr b
    PmVar :: String -> PmExpr a
    PmRawE :: String -> PmExpr a
    PmScalar :: PmScalar s => s -> PmExpr s

renderPmScalarType :: forall s. PmScalar s => s -> Doc
renderPmScalarType _ = text (pmScalarTypeName (undefined :: s))

renderPmExpr :: forall a. PmExpr a -> Doc
renderPmExpr e = case e of
                      PmMatrix (xss :: [[PmExpr s]]) -> 
                        parens $ text "new Matrix<" <> renderPmScalarType (undefined :: s) <> text ">" 
                                        <> parens (lbracket </> 
                                                    (indent 4 . vsep . punctuate comma $ 
                                                        [ lbracket 
                                                            <> hsep (intersperse comma $ 
                                                                        renderPmExpr `fmap` xs) 
                                                            <> rbracket
                                                         | xs <- xss ])
                                                   </> rbracket)
                      PmPolytope (ie :: PmExpr [[s]]) eq -> 
                        parens $ text "new Polytope<" <> renderPmScalarType (undefined :: s) <> text ">" 
                                            <> (parens . align) 
                                                     (text "INEQUALITIES =>" <+> renderPmExpr ie
                                                       <$$> comma <+> text "EQUATIONS =>" <+> renderPmExpr eq)
                      PmGet e' prop -> renderPmExpr e' <> text "->" <> renderPmProp prop
                      PmVar x -> PP.char '$' <> text x
                      PmRawE x -> PP.string x
                      PmScalar x -> PP.string (pmScalarShow x)

data PmProperty = VERTICES deriving Show

renderPmProp :: PmProperty -> Doc
renderPmProp = text . show


data PmStmt where
    PmAssign :: String -> PmExpr a -> PmStmt
    PmPrint :: PmExpr a -> PmStmt
    PmRawS :: String -> PmStmt
    PmSequence :: [PmStmt] -> PmStmt

instance Monoid PmStmt where
    mempty = PmSequence []
    s1 `mappend` s2 = PmSequence [s1,s2]

renderPmStmt :: PmStmt -> Doc
renderPmStmt (PmAssign x y) = text "my" <+> PP.char '$' <> text x <+> PP.char '=' <+> renderPmExpr y <> semi
renderPmStmt (PmPrint x) = text "print" <> parens (renderPmExpr x) <> semi
renderPmStmt (PmRawS x) = PP.string x
renderPmStmt (PmSequence xs) = vsep (renderPmStmt `fmap` xs) 


runPolymake ::  MonadIO m => PmStmt -> m [[String]]
runPolymake = runPolymakeScript . prettyString . renderPmStmt

runPolymakeScript ::  MonadIO m => String -> m [[String]]
runPolymakeScript script = liftIO $ do
    rnd <- randomIO :: IO Int
    tmpdir <- getTemporaryDirectory
    let script' = "use application \"polytope\";\n"++script 
        tmpfile = tmpdir File.</> show (abs rnd) File.<.> ".poly"
    putStrLn tmpfile
    writeFile tmpfile script'
    putStrLn . prettyString $ text "script =" <$$> indent 2 (dullwhite $ PP.string script') 
    res <- readProcess "polymake" ["--script",tmpfile] ""
    return . fmap words . lines $ res


pmMatrix' ::  PmScalar s => [[s]] -> PmExpr [[s]]
pmMatrix' = PmMatrix . (fmap . fmap) PmScalar
    


test_StandardTwoSimplex :: PmScalar s => IO [[s]]
test_StandardTwoSimplex = polymakeGetVertices'
                                          [[0,1,0,0]
                                          ,[0,0,1,0]
                                          ,[0,0,0,1]]

                                          [[-1,1,1,1]]


polymakeGetVertices :: (MonadIO m, PmScalar s) => [[s]] -> [[s]] -> m [[String]] 
polymakeGetVertices inequalities equations = do 
    res <- runPolymake $ 
            PmPrint ((PmPolytope { 
                pmInequalities =  pmMatrix' inequalities
              , pmEquations = pmMatrix' equations } )
                     `PmGet` VERTICES)

    return res

polymakeGetVertices' :: (MonadIO m, PmScalar s) => [[s]] -> [[s]] -> m [[s]] 
polymakeGetVertices' inequalities equations = do
    res <- polymakeGetVertices inequalities equations 
    return (  (fmap . fmap) (either (error . show) id . parse pmScalarParse "" ) res  )


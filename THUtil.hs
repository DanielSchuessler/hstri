{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances, TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module THUtil(
    module Debug.Trace,
    Pretty,
    mkConstantDecls,
    showVars,
    Printees(..),
    showExps,
    traceExps,
    ppTestCase,
    assrt,
    stringQuote,
    printQuote,
    liftFunction,
    straceExp,
    ltraceExp,
    problem,
    debugIndex
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Util
import Quote
import Data.List
import Debug.Trace
import PrettyUtil
import Control.Arrow((&&&))
import Test.QuickCheck
import qualified Data.Map as M
import OrphanInstances.Lift()
import Element
import Data.Generics
import Data.Maybe
import FileLocation
import qualified Data.Vector.Generic as VG

atType ::  TypeQ -> ExpQ
atType t = [| \f -> f (undefined :: $(t)) |]




mkConstantDecls :: Quote a => [a] -> (a -> ExpQ) -> Q [Dec]
mkConstantDecls xs lift_ =
    concatMapM (\x ->
        let n = mkName (quote x)
        in
            sequence [ valD (varP n) (normalB (lift_ x)) [] ] 
        )

            xs


showVar :: Name -> Q Exp
showVar n = [| s ++ show $(varE n) |] 
    where
        s = nameBase n ++ " = "


showVars :: [Name] -> Q Exp
showVars ns = 
    [| "{ " ++ $(x) ++ " }" |]
  where
    x = foldr1 (\e e' -> [| $(e) ++ ", " ++ $(e') |]) (fmap showVar ns)


class Printees a where
    toLabelExpressionPairs :: a -> Q [(String,Exp)]

instance Printees () where toLabelExpressionPairs = const (return [])
instance Printees Name where toLabelExpressionPairs = toLabelExpressionPairs . (:[])
instance Printees [Name] where toLabelExpressionPairs = return . map (nameBase &&& VarE)
instance Printees ExpQ where toLabelExpressionPairs = toLabelExpressionPairs . (:[])
instance Printees [ExpQ] where toLabelExpressionPairs = toLabelExpressionPairs . sequence
instance Printees (Q [Exp]) where toLabelExpressionPairs = fmap (map (pprint &&& id))
instance Printees (Q [(String,Exp)]) where toLabelExpressionPairs = id


toLabelExpressionPairsE :: Printees a => a -> ExpQ
toLabelExpressionPairsE = fmap f . toLabelExpressionPairs
    where
        f = ListE . fmap g
        g (s,e) = TupE [ LitE (StringL s)
                       , AppE (VarE 'pretty) e
                       ]
    

assrt :: Printees a => ExpQ -> a -> Q Exp
assrt e printees = do
    e' <- e

    let 
        msg = "Assertion failed: "++pprint e' 


    [| if $(return e')
          then id
          else error (msg
                        ++"\n"
                        ++docToString (prettyEqs $(toLabelExpressionPairsE printees))) |]

    
showExps :: Printees a => a -> Q Exp
showExps printees =

    [| docToString (prettyEqs $(toLabelExpressionPairsE printees)) |]

traceExps :: Printees a => [Char] -> a -> Q Exp
traceExps msg printees = [| trace (msg ++ " " ++ $(showExps printees)) |]

ppTestCase :: Printees a => a -> ExpQ
ppTestCase printees = [| printTestCase $(showExps printees) |]

unqual :: Name -> Name
unqual = mkName . nameBase 

stringQuote :: Lift t => Bool -> t -> Q Exp
stringQuote qual qx = do
        x <- lift qx
        lift 
            . pprint 
            . (if qual then id else everywhere (mkT unqual))
            . everywhere tExp 
            $ x

 where
        tExp = mkT (\t -> fromMaybe t $ charListToStringLit t)


printQuote :: Lift a => a -> ExpQ
printQuote = appE [|putStrLn|] . stringQuote False

-- | 'lift' a function with finite domain
liftFunction
  :: (Ord (Element dom), Lift (Element dom), Lift y, AsList dom) =>
     dom -> (Element dom -> y) -> Q Exp
liftFunction dom f = [| (M.!) $(lift . M.fromList . map (id &&& f) . asList $ dom) |] 
    

charListToStringLit :: Exp -> Maybe Exp
charListToStringLit e = do
    ListE xs <- Just e
    cs <- mapM (\x -> do
                    LitE (CharL c) <- Just x
                    Just c)
               xs

    Just (LitE (StringL cs)) 


ltraceExp :: [Char] -> ExpQ -> ExpQ
ltraceExp msg e = traceExps msg e `appE` e

straceExp :: ExpQ -> ExpQ
straceExp = ltraceExp ""

problem :: Q Exp
problem = [| error . ($(lift . (++ ": ") . locationToString =<< location) ++) |]


debugIndex :: Q Exp
debugIndex = do
    l <- location
    [| \_v _i -> case _v VG.!? _i of
                    Just _x -> _x
                    Nothing -> error ($(lift . (++ ": Index out of bounds") . locationToString $ l)) |] 


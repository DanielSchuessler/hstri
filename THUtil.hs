{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances, TemplateHaskell #-}
module THUtil(
    module Debug.Trace,
    Pretty,
    liftByShow,
    mkConstantDecls,
    showVars,
    Printees(..),
    showExps,
    traceExps,
    ppTestCase,
    assrt) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Word
import Data.BitSet.Word8
import Util
import Quote
import Data.List
import Debug.Trace
import PrettyUtil
import Control.Applicative
import Control.Arrow((&&&))
import Test.QuickCheck

atType ::  TypeQ -> ExpQ
atType t = [| \f -> f (undefined :: $(t)) |]


instance Lift Word8 where
    lift w = sigE (litE (IntegerL (toInteger w))) [t| Word8 |]

instance Lift (BitSet a) where
    lift (BitSet w) = [| BitSet $(lift w) |]

-- | Suitable for types with only nullable constructors and derived Show instance
liftByShow ::  Show a => a -> ExpQ
liftByShow = conE . mkName . show

-- -- | Suitable for types whose 'Quote' instance returns just a variable name
-- liftByShow ::  Show a => a -> ExpQ
-- liftByShow = conE . mkName . show

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

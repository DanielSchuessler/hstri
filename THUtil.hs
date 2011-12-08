{-# LANGUAGE TemplateHaskell #-}
module THUtil(
    module Debug.Trace,
    liftByShow,
    mkConstantDecls,
    showVars,
    prVars,
    prVars') where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Word
import Data.BitSet.Word8
import Util
import Quote
import Data.List
import Debug.Trace
import PrettyUtil

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

prVars :: [Name] -> Q Exp
prVars ns = 
    [| prettyEqs $(x) |]
  where
    x = listE (fmap (\n ->
                        [| ( $(lift (nameBase n)), pretty $(varE n) )  |])
                    ns)

prVars' :: [Name] -> Q Exp
prVars' ns = [| docToString $(prVars ns) |]

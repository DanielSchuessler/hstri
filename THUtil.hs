{-# LANGUAGE ViewPatterns, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances, TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module THUtil(
    module Debug.Trace,
    module FileLocation,
    
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
    inherit,inheritPretty,inheritShow,
    inheritSingleArgClass,
    -- * Error locations
    liftedLocationString,
    debugIndex,
    -- ** Map-related functions
    fromListNoCollision,
    insertNoCollision,

    -- ** Reex
    Convertible,
    ExpQ,
    TypeQ,Q,Dec
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Util
import Quote
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
import EitherC
import THBuild
import Control.Monad

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
    loc <- location

    let 
        msg = locationToString loc ++ ": Assertion failed: "++ pprint e' 


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
liftFunction dom f =
    [| flip $(indx) $(lift . M.fromList . map (id &&& f) . asList $ dom) |] 
    

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



debugIndex :: Q Exp
debugIndex = do
    l <- location
    [| \_v _i -> case _v VG.!? _i of
                    Just _x -> _x
                    Nothing -> error ($(lift . (++ ": Index out of bounds") . locationToString $ l)) |] 




fromListNoCollision :: Q Exp
fromListNoCollision =
    [| M.fromListWith (\_ _ -> error ($liftedLocationString ++ ": fromListNoCollision: Collision")) |]

insertNoCollision :: Q Exp
insertNoCollision =
    [| M.insertWith (\_ _ -> error ($liftedLocationString ++ ": insertNoCollision: Collision")) |] 



-- ClassD Cxt Name [TyVarBndr] [FunDep] [Dec]

inherit
  :: (Convertible accessor ExpQ, Convertible sub TypeQ, Convertible super TypeQ) =>
     (TypeQ -> Q (Name, [Type]))
     -> [Name] -> [Name] -> sub -> super -> accessor -> Q [Dec]
inherit cls methods asstysyns (typeQ -> sub) (typeQ -> super) accessor = do
    (cls_n, cls_tys) <- cls sub 
    (cls_n', cls_tys') <- cls super 

    sequence $ [
        instanceD 
            (cxt [return (ClassP cls_n' cls_tys')])
            (return (foldl AppT (ConT cls_n) cls_tys))
            (
            [
                svalD (varP m) ('(.) `sappE` varE m `sappE` accessor)

                | m <- methods
            
            ]
            ++
            [
                tySynInstD asstysyn (map return cls_tys) 
                    (return (foldl AppT (ConT asstysyn) cls_tys'))

                    | asstysyn <- asstysyns
            ]
            )
        ]

        
inheritSingleArgClass
  :: (Convertible accessor ExpQ,
      Convertible sub TypeQ,
      Convertible super TypeQ) =>
     Name -> [Name] -> [Name] -> sub -> super -> accessor -> Q [Dec]
inheritSingleArgClass clsn = inherit (liftM (\t -> (clsn,[t])))

inheritShow
  :: (Convertible accessor ExpQ,
      Convertible sub TypeQ,
      Convertible super TypeQ) =>
     sub -> super -> accessor -> Q [Dec]
inheritShow = inheritSingleArgClass ''Show ['show] []
inheritPretty
  :: (Convertible accessor ExpQ,
      Convertible sub TypeQ,
      Convertible super TypeQ) =>
     sub -> super -> accessor -> Q [Dec]
inheritPretty = inheritSingleArgClass ''Pretty ['pretty] []


{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances, TemplateHaskell #-}
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
    -- * Error locations
    liftedLocationString,
    debugIndex,
    -- ** Map-related functions
    fromListNoCollision,
    insertNoCollision,
    -- * Builders
    ToExpQ(..),ToPatQ(..),ToTypeQ(..),
    svalD,smatch,slamE,getFieldE,sappT
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
import Control.Applicative
import Data.Char

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



locationString :: Q String
locationString = locationToString <$> location

liftedLocationString :: Q Exp
liftedLocationString = lift =<< locationString

fromListNoCollision :: Q Exp
fromListNoCollision =
    [| M.fromListWith (\_ _ -> error ($liftedLocationString ++ ": fromListNoCollision: Collision")) |]

insertNoCollision :: Q Exp
insertNoCollision =
    [| M.insertWith (\_ _ -> error ($liftedLocationString ++ ": insertNoCollision: Collision")) |] 


isUpperName :: Name -> Bool
isUpperName = isUpper . head . nameBase

ifUpperThenElse :: (Name -> t) -> (Name -> t) -> Name -> t
ifUpperThenElse ku kl n = (if isUpperName n then ku else kl) n

class ToExpQ a where
    expQ :: a -> ExpQ 

instance ToExpQ ExpQ where expQ = id
instance ToExpQ Name where expQ = ifUpperThenElse conE varE
instance ToExpQ String where expQ = expQ . name


class ToPatQ a where
    patQ :: a -> PatQ

instance ToPatQ PatQ where patQ = id
instance ToPatQ Name where patQ = ifUpperThenElse (flip conP []) varP
instance ToPatQ String where patQ = patQ . name

class ToPatsQ a where
    patsQ :: a -> [PatQ]

instance ToPatsQ [PatQ] where patsQ = id
instance ToPatsQ PatQ where patsQ = return
instance ToPatsQ Name where patsQ = return . patQ
instance ToPatsQ String where patsQ = return . patQ

class ToTypeQ a where
    typeQ :: a -> TypeQ

instance ToTypeQ TypeQ where typeQ = id
instance ToTypeQ Name  where typeQ = ifUpperThenElse conT varT
instance ToTypeQ String where typeQ = typeQ . name

class ToName a where
    name :: a -> Name

instance ToName Name where name = id
instance ToName String where name = mkName

svalD :: (ToPatQ a, ToExpQ a1) => a -> a1 -> DecQ
svalD p e = valD (patQ p) (normalB (expQ e)) []

smatch :: (ToPatQ a, ToExpQ a1) => a -> a1 -> MatchQ
smatch p e = match (patQ p) (normalB (expQ e)) []

slamE :: (ToPatsQ a, ToExpQ a1) => a -> a1 -> ExpQ
slamE p e = lamE (patsQ p) (expQ e)

getFieldE :: (ToName a) => 
    a       -- ^ Ctor name
    -> Int  -- ^ Ctor arity
    -> Int  -- ^ 0-based index of field to get
    -> Q Exp
getFieldE ctor n i = do
    x <- newName "_x"
    slamE 
        (conP (name ctor) (map (\j -> if i==j then varP x else wildP) [0..n-1]))     
        x

sappT :: (ToTypeQ a, ToTypeQ a1) => a -> a1 -> TypeQ
sappT x y = typeQ x `appT` typeQ y


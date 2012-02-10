{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module THBuild where

import Language.Haskell.TH
import Data.Char

isUpperName :: Name -> Bool
isUpperName = isUpper . head . nameBase

ifUpperThenElse :: (Name -> t) -> (Name -> t) -> Name -> t
ifUpperThenElse ku kl n = (if isUpperName n then ku else kl) n


class Convertible a b where
    convert :: a -> b

expQ :: Convertible a ExpQ => a -> ExpQ
expQ = convert 

expQs :: Convertible a [ ExpQ ] => a -> [ ExpQ ]
expQs = convert 

patQ :: Convertible a PatQ => a -> PatQ
patQ = convert
patQs :: Convertible a [PatQ] => a -> [PatQ]
patQs = convert
typeQ :: Convertible a TypeQ => a -> TypeQ
typeQ = convert
typeQs :: Convertible a [ TypeQ ] => a -> [ TypeQ ]
typeQs = convert
name :: Convertible a Name => a -> Name
name = convert
tyVarBndr :: Convertible a TyVarBndr => a -> TyVarBndr
tyVarBndr = convert

conQ :: Convertible a ConQ => a -> ConQ
conQ = convert

cxtQ :: Convertible a CxtQ => a -> CxtQ
cxtQ = convert

strictTypeQ :: Convertible a StrictTypeQ => a -> StrictTypeQ
strictTypeQ = convert

strictTypeQs :: Convertible a [StrictTypeQ] => a -> [StrictTypeQ]
strictTypeQs = convert

instance Convertible ExpQ ExpQ where convert = id
instance Convertible Name ExpQ where convert = ifUpperThenElse conE varE
instance Convertible String ExpQ where convert = expQ . name
instance Convertible Lit ExpQ where convert = litE 
instance Convertible Integer ExpQ where convert = litE . integerL 

instance Convertible [ ExpQ ] [ ExpQ ] where convert = id
instance Convertible [ Name ] [ ExpQ ] where convert = map expQ
instance Convertible [ String ] [ ExpQ ] where convert = map expQ

instance Convertible PatQ PatQ where convert = id
instance Convertible Name PatQ where convert = ifUpperThenElse (flip conP []) varP
instance Convertible String PatQ where convert = patQ . name


instance Convertible [PatQ] [PatQ] where convert = id
instance Convertible [ Name ] [PatQ] where convert = map patQ
instance Convertible [ String ] [PatQ] where convert = map patQ
instance Convertible PatQ [PatQ] where convert = return
instance Convertible Name [PatQ] where convert = return . patQ
instance Convertible String [PatQ] where convert = return . patQ


instance Convertible TypeQ TypeQ where convert = id
instance Convertible Name TypeQ where convert = ifUpperThenElse conT varT
instance Convertible String TypeQ where convert = typeQ . name

instance Convertible [TypeQ] [TypeQ] where convert = id
instance Convertible TypeQ [TypeQ] where convert = return
instance Convertible Name [TypeQ] where convert = return . typeQ
instance Convertible String [TypeQ] where convert = return . typeQ

instance Convertible Name Name where convert = id
instance Convertible String Name where convert = mkName


instance Convertible TyVarBndr TyVarBndr where convert = id
instance Convertible Name TyVarBndr where convert = PlainTV
instance Convertible String TyVarBndr where convert = tyVarBndr . name

instance Convertible ConQ ConQ where convert = id

instance Convertible CxtQ CxtQ where convert = id
instance Convertible [PredQ] CxtQ where convert = sequence

instance Convertible StrictTypeQ StrictTypeQ where convert = id
instance Convertible TypeQ StrictTypeQ where convert = strictType notStrict
instance Convertible Name StrictTypeQ where convert = strictTypeQ . typeQ
instance Convertible String StrictTypeQ where convert = strictTypeQ . typeQ

instance Convertible [ StrictTypeQ ] [ StrictTypeQ ] where convert = id
instance Convertible StrictTypeQ [StrictTypeQ] where convert = return
instance Convertible TypeQ [StrictTypeQ] where convert = return . strictTypeQ
instance Convertible Name [StrictTypeQ] where convert = return . strictTypeQ
instance Convertible String [StrictTypeQ] where convert = return . strictTypeQ 


svalD
  :: (Convertible a PatQ, Convertible a1 ExpQ) => a -> a1 -> DecQ
svalD p e = valD (patQ p) (normalB (expQ e)) []

smatch
  :: (Convertible a PatQ, Convertible a1 ExpQ) => a -> a1 -> MatchQ
smatch p e = match (patQ p) (normalB (expQ e)) []

slamE
  :: (Convertible a [PatQ], Convertible a1 ExpQ) => a -> a1 -> ExpQ
slamE p e = lamE (patQs p) (expQ e)

getFieldE :: (Convertible a Name) => 
    a       -- ^ Ctor name
    -> Int  -- ^ Ctor arity
    -> Int  -- ^ 0-based index of field to get
    -> Q Exp
getFieldE ctor n i = do
    x <- newName "_x"
    slamE 
        (conP (name ctor) (map (\j -> if i==j then varP x else wildP) [0..n-1]))     
        x

sappT
  :: (Convertible a TypeQ, Convertible a1 TypeQ) => a -> a1 -> TypeQ
sappT x y = typeQ x `appT` typeQ y


(&) ::  Convertible a1 a => a1 -> [a] -> [a]
a & b = convert a : b
infixr 5 &

snewtypeD
  :: (Convertible a CxtQ,
      Convertible a1 Name,
      Convertible a2 ConQ) =>
     a -> a1 -> [TyVarBndr] -> a2 -> [Name] -> DecQ
snewtypeD c na bndrs con derivs = newtypeD (cxtQ c) (name na) bndrs (conQ con) derivs

snormalC
  :: (Convertible a Name, Convertible a1 [StrictTypeQ]) =>
     a -> a1 -> ConQ
snormalC na _strictTypeQs = normalC (name na) (strictTypeQs _strictTypeQs)

sappE
  :: (Convertible a ExpQ, Convertible a1 ExpQ) => a -> a1 -> ExpQ
sappE x y = expQ x `appE` expQ y


stupE :: Convertible a [ExpQ] => a -> ExpQ
stupE = tupE . expQs

stupP :: Convertible a [PatQ] => a -> PatQ
stupP = tupP . patQs

(\->) :: forall a a1.
                        (Convertible a [PatQ], Convertible a1 ExpQ) =>
                        a -> a1 -> ExpQ
(\->) = slamE 

infixr 0 \->

sconP
  :: (Convertible a Name, Convertible a1 [PatQ]) => a -> a1 -> PatQ
sconP n ps = conP (name n) (patQs ps)

sclassP
  :: (Convertible a Name, Convertible a1 [TypeQ]) => a -> a1 -> PredQ
sclassP na tys = classP (name na) (typeQs tys)

stySynInstD
  :: (Convertible a Name,
      Convertible a1 [TypeQ],
      Convertible a2 TypeQ) =>
     a -> a1 -> a2 -> DecQ
stySynInstD na tys ty = tySynInstD (name na) (typeQs tys) (typeQ ty)

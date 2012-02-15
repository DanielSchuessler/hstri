{-# LANGUAGE UndecidableInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, BangPatterns, NoMonomorphismRestriction, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, NamedFieldPuns, FlexibleContexts, TypeFamilies, OverlappingInstances#-} 
{-# OPTIONS -Wall #-}

-- | Utility functions for Patrick Bahr's /equivalence/ package
module Equivalence.Class(
    module EitherC,
    module Element,
    IsEquivalenceClass(..),
    EquivalenceClassOf,
    IsEquivalence(..), 
    EnumerableEquivalence(..),
    eqvRep_safe, 
    eqvRep, 
    eqvEquivalents,
    eqvClassOf,
    prettyClass,
    prettyEquivalence,
    ecMap,
    -- * Universal impls
    EqvClassImpl(..), toEqvClassImpl,
    EqvImpl(..), toEqvImpl,
    EnumEqvImpl(..), enumEqvImpl, toEnumEqvImpl,
    -- * Basic impls
    TrivialEquivalenceClass(..),
    TrivialEquivalence,
    trivialEquivalence,
    ecProduct,
    eqvProduct,
    -- * Testing
    polyprop_EquivalenceClass,
    polyprop_Equivalence,
    polyprop_Equivalence',
    polyprop_respects,
    polyprop_respects2
    )   where

import Element
import Control.Applicative
import QuickCheckUtil
import Test.QuickCheck
import PrettyUtil
import EitherC


class AsList cls => IsEquivalenceClass cls where
    canonicalRep :: cls -> Element cls
    ecSize :: cls -> Int

type family EquivalenceClassOf er :: * 

polyprop_EquivalenceClass
  :: (Eq (Element cls), IsEquivalenceClass cls) => cls -> Property
polyprop_EquivalenceClass ec = 
        conjoin' [
            property (elem (canonicalRep ec) l)
          , ecSize ec .=. length l
          ]

    where
        l = asList ec


class   (   IsEquivalenceClass (EquivalenceClassOf er)
        ,   Element er ~ Element (EquivalenceClassOf er)
        ,   Eq (Element er)) => IsEquivalence er where

    eqvClassOf_safe :: er -> Element er -> AttemptC (EquivalenceClassOf er)

    eqvEquivalent :: er -> Element er -> Element er -> Bool
    eqvEquivalent er x y = eqvRep er x == eqvRep er y

-- | Throws an error if the element is not in the domain of the equivalence
eqvClassOf :: IsEquivalence er => er -> Element er -> EquivalenceClassOf er
eqvClassOf er = $unEitherC ("eqvClassOf: Element is not in the equivalence relation") 
    . eqvClassOf_safe er

polyprop_Equivalence
  :: (Ord (Element (EquivalenceClassOf er)),
      Show (Element (EquivalenceClassOf er)),
      IsEquivalence er) =>
     er -> Gen (Element (EquivalenceClassOf er)) -> Property
polyprop_Equivalence er dom =
    forAll dom
        (\x ->
            let
                cls_x = eqvClassOf er x
            in
                conjoin' [
                      printTestCase "Check class" $ polyprop_EquivalenceClass cls_x
                    , 
                      forAllElements (asList cls_x) 
                        (\x' -> 
                            printTestCase "Class of elements of class" $ 
                            clsEql cls_x (eqvClassOf er x'))
                    , printTestCase "Reflexivity" (eqv x x)
                    , forAll dom (\y -> printTestCase "Symmetry" $ eqv x y .=. eqv y x)
                    , forAll2 dom dom (\y z -> printTestCase "Transitivity" $ 
                        if eqv x y && eqv y z 
                           then eqv x z 
                           else True)

                    , forAll dom (\y -> 
                        printTestCase "Consistency of eqvEquivalent,canonicalRep,eqvClassOf" $ 
                        let e = eqvEquivalent er x y
                        in 
                            e .=. (eqvRep er x == eqvRep er y))
                    ])

  where
    eqv = eqvEquivalent er
  

    clsEql ec1 ec2 = 
        canonicalRep ec1 .=. canonicalRep ec2 .&.
        setEq (asList ec1) (asList ec2)


polyprop_Equivalence'
  :: (Ord (Element (EquivalenceClassOf er)),
      Show (Element (EquivalenceClassOf er)),
      IsEquivalence er) =>
     er -> [Element (EquivalenceClassOf er)] -> Property
polyprop_Equivalence' er = polyprop_Equivalence er . elements 

class IsEquivalence er => EnumerableEquivalence er where
    eqvClasses :: er -> [EquivalenceClassOf er]


eqvRep_safe :: IsEquivalence er => er -> Element er -> AttemptC (Element er)
eqvRep_safe e x = canonicalRep <$> eqvClassOf_safe e x 

eqvRep :: IsEquivalence er => er -> Element er -> Element er
eqvRep = ($unEitherC "eqvRep" .) . eqvRep_safe

eqvEquivalents
  :: IsEquivalence er => er -> Element er -> [Element er]
eqvEquivalents e x = asList $ eqvClassOf e x



data EqvClassImpl elt = EqvClassImpl {
    eci_elements :: [elt]
,   eci_canonicalRep :: elt
,   eci_Size :: Int
}
    deriving Show

toEqvClassImpl :: IsEquivalenceClass xs => xs -> EqvClassImpl (Element xs)
toEqvClassImpl = EqvClassImpl <$> asList <*> canonicalRep <*> ecSize

type instance Element (EqvClassImpl elt) = elt

instance AsList (EqvClassImpl elt) where asList = eci_elements

instance IsEquivalenceClass (EqvClassImpl elt) where
    canonicalRep = eci_canonicalRep
    ecSize = eci_Size

newtype EqvImpl cls = EqvImpl { 
    eqvi_ClassOf_safe :: Element cls -> AttemptC cls 
} 

toEqvImpl :: IsEquivalence er => er -> EqvImpl (EquivalenceClassOf er)
toEqvImpl = EqvImpl <$> eqvClassOf_safe

type instance Element (EqvImpl cls) = Element cls
type instance EquivalenceClassOf (EqvImpl cls) = cls

instance (Eq (Element cls), IsEquivalenceClass cls) => IsEquivalence (EqvImpl cls) where
    eqvClassOf_safe = eqvi_ClassOf_safe

data EnumEqvImpl cls = EnumEqvImpl { 
    eeqvi_eqvi :: EqvImpl cls
,   eeqvi_Classes :: [cls] 
} 

toEnumEqvImpl :: EnumerableEquivalence er => er -> EnumEqvImpl (EquivalenceClassOf er)
toEnumEqvImpl = EnumEqvImpl <$> toEqvImpl <*> eqvClasses

type instance Element (EnumEqvImpl cls) = Element cls
type instance EquivalenceClassOf (EnumEqvImpl cls) = cls

instance (Eq (Element cls), IsEquivalenceClass cls) => IsEquivalence (EnumEqvImpl cls) where
    eqvClassOf_safe = eqvClassOf_safe . eeqvi_eqvi

instance (Eq (Element cls), IsEquivalenceClass cls) => EnumerableEquivalence (EnumEqvImpl cls) where
    eqvClasses = eeqvi_Classes


newtype TrivialEquivalenceClass a = 
    TrivialEquivalenceClass { runTrivialEquivalenceClass :: a }

type instance Element (TrivialEquivalenceClass a) = a
instance AsList (TrivialEquivalenceClass a) where asList = (:[]) . runTrivialEquivalenceClass

instance Eq a => IsEquivalenceClass (TrivialEquivalenceClass a) where 
    ecSize = const 1
    canonicalRep = runTrivialEquivalenceClass

type TrivialEquivalence a = EqvImpl (TrivialEquivalenceClass a)

trivialEquivalence :: TrivialEquivalence a
trivialEquivalence = EqvImpl (return . TrivialEquivalenceClass)


polyprop_respects
  :: (Eq (Element (EquivalenceClassOf er1)),
      Show (Element (EquivalenceClassOf er1)),
      Show (Element (EquivalenceClassOf er2)),
      IsEquivalence er1,
      IsEquivalence er2) =>
     er1
     -> er2
     -> Gen (Element (EquivalenceClassOf er1))
     -> (Element (EquivalenceClassOf er1)
         -> Element (EquivalenceClassOf er2))
     -> Property
polyprop_respects er1 er2 dom f =
    forAll dom (\x ->

        let
            fx = f x
        in

         printTestCase (unlines [
                                "=== FUNCTION VALUE AT FIRST ELEMENT ==="
                               ,show fx ]) $

         forAllElements (asList (eqvClassOf er1 x))
            (\x' ->
                let
                    fx' = f x'
                in
                    classify (x==x') ("trivial (same representative)") $ 
                        printTestCase (unlines 
                               ["=== FUNCTION VALUE AT SECOND ELEMENT ==="
                               ,show fx' ]) $
                    
                                    (eqvEquivalent er2 fx fx')))



ecProduct
  :: (IsEquivalenceClass cls, IsEquivalenceClass cls1) =>
     cls -> cls1 -> EqvClassImpl (Element cls, Element cls1)
ecProduct ec1 ec2 = 
    EqvClassImpl 
        (asList ec1 `cart` asList ec2) 
        (canonicalRep ec1, canonicalRep ec2)
        (ecSize ec1 * ecSize ec2)

eqvProduct
  :: (IsEquivalence er, IsEquivalence er1) =>
     er -> er1 -> EqvImpl (EqvClassImpl (Element er, Element er1))
eqvProduct er1 er2 = EqvImpl (\(x1,x2) -> liftA2 ecProduct (eqvClassOf_safe er1 x1) (eqvClassOf_safe er2 x2))


polyprop_respects2
  :: (Eq (Element (EquivalenceClassOf er1)),
      Eq (Element (EquivalenceClassOf er1')),
      Show (Element (EquivalenceClassOf er1)),
      Show (Element (EquivalenceClassOf er1')),
      Show (Element (EquivalenceClassOf er2)),
      IsEquivalence er2,
      IsEquivalence er1,
      IsEquivalence er1') =>
     er1
     -> er1'
     -> er2
     -> Gen (Element (EquivalenceClassOf er1))
     -> Gen (Element (EquivalenceClassOf er1'))
     -> (Element (EquivalenceClassOf er1)
         -> Element (EquivalenceClassOf er1')
         -> Element (EquivalenceClassOf er2))
     -> Property
polyprop_respects2 er1 er1' er2 dom1 dom2 f =
    polyprop_respects (eqvProduct er1 er1') er2 (cart dom1 dom2) (uncurry f)


prettyClass :: (AsList a, Pretty (Element a)) => a -> Doc
prettyClass = prettyListAsSet . asList

prettyEquivalence
  :: (EnumerableEquivalence er, Pretty (EquivalenceClassOf er)) =>
     er -> Doc
prettyEquivalence e = vsep 
            (text "Equivalence, classes = {"
             : fmap (indent 2 . pretty) (eqvClasses e) 
             ++ [rbrace])


instance (Pretty elt) => Pretty (EqvClassImpl elt) where
    pretty = prettyClass

instance (Pretty cls, Eq (Element cls), IsEquivalenceClass cls) => Pretty (EnumEqvImpl cls) where
    pretty = prettyEquivalence


enumEqvImpl :: (Element cls -> AttemptC cls) -> [cls] -> EnumEqvImpl cls
enumEqvImpl = EnumEqvImpl . EqvImpl

ecMap
  :: IsEquivalenceClass cls =>
     (Element cls -> elt) -> cls -> EqvClassImpl elt
ecMap f c =
    EqvClassImpl
                                (fmap f (asList c))
                                (f (canonicalRep c))
                                (ecSize c)



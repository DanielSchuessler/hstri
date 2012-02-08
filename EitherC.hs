{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, ScopedTypeVariables, PolymorphicComponents, DeriveFunctor #-}
-- | Description: CPS 'Either'.
module EitherC where
import Control.Applicative
import Data.SumType
import Control.Exception
import Language.Haskell.TH
import FileLocation
import Language.Haskell.TH.Syntax
import Data.Typeable(Typeable)
import THUtil

newtype EitherC e a = EitherC { runEitherC :: forall r. (e -> r) -> (a -> r) -> r }
    deriving Functor

instance Applicative (EitherC e) where
    pure a = EitherC (\_ ks -> ks a) 
    mf <*> mx = EitherC (\ke ks -> runEitherC mf ke
                                    (\f -> runEitherC mx ke
                                        (\x -> ks (f x))))

instance Monad (EitherC e) where
    return = pure
    ma >>= f = EitherC (\ke ks -> runEitherC ma ke 
                                    (\b -> runEitherC (f b) ke ks)) 

type instance L (EitherC e a) = e 
type instance R (EitherC e a) = a 


instance SuperSumTy (EitherC e a) where
    left' = failureWithoutLoc 
    right' = pure

instance SubSumTy (EitherC e a) where
    either' ke ks ma = runEitherC ma ke ks

instance SumType (EitherC e a)

consolidateEitherC :: EitherC e a -> EitherC e a
consolidateEitherC = either' failureWithoutLoc pure

eitherCToEither :: EitherC e a -> Either e a
eitherCToEither = toEither


unEitherCWithLoc :: Show e => [Char] -> EitherC e a -> a
unEitherCWithLoc loc ma = runEitherC ma (\e -> error (loc ++ ": unEitherC: "++show e)) id

unEitherC :: ExpQ
unEitherC = [| unEitherCWithLoc $liftedLocationString |]

eitherCFromEither :: Either e a -> EitherC e a
eitherCFromEither = fromEither

instance (Show e, Show a) => Show (EitherC e a) where
    showsPrec prec x = showParen (prec > 10) 
        (showString "eitherCFromEither " . showsPrec 11 (eitherCToEither x))

data Located e = Located String e
    deriving (Typeable)

instance Show e => Show (Located e) where
    show (Located l e) = l ++ ": " ++ show e

instance Exception e => Exception (Located e)

failureWithoutLoc :: e -> EitherC e a
failureWithoutLoc e = EitherC (\ke _ -> ke e)

failureWithLoc :: String -> e -> EitherC (Located e) a
failureWithLoc l e = failureWithoutLoc (Located l e)

failure :: Q Exp
failure = [| failureWithLoc $liftedLocationString |] 

type LErrorCall = Located ErrorCall

idLErrorCall :: EitherC LErrorCall a -> EitherC LErrorCall a
idLErrorCall = id

failureStr :: Q Exp
failureStr = [| idLErrorCall . $failure . ErrorCall |]

-- | Catches and rethrows with a message prepended
--
-- @$(wrapFailureStr) :: Show e => [Char] -> EitherC e a -> EitherC LErrorCall a@
wrapFailureStr :: Q Exp
wrapFailureStr = 
    [| \_newMsg _x -> 
            idLErrorCall (
                runEitherC _x 
                    (\_err -> $failureStr (_newMsg++"\n\tInner error: "++show _err))
                    return
                    )
                    
                    |]
                

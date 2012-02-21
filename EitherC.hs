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

locationString :: Q String
locationString = locationToString <$> location

liftedLocationString :: Q Exp
liftedLocationString = lift =<< locationString


newtype EitherC e a = EitherC { runEitherC :: forall r. (e -> r) -> (a -> r) -> r }
    deriving Functor

-- | Convenient argument reordering of 'runEitherC'
runEitherC' :: (e -> r) -> (a -> r) -> EitherC e a -> r
runEitherC' ke ks x = runEitherC x ke ks

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


-- unEitherCWithLoc :: Show e => [Char] -> EitherC e a -> a
-- unEitherCWithLoc loc ma = runEitherC ma (\e -> error (loc ++ ": unEitherC: "++show e)) id
-- 
-- unEitherC :: ExpQ
-- unEitherC = [| unEitherCWithLoc $liftedLocationString |]

eitherCFromEither :: Either e a -> EitherC e a
eitherCFromEither = fromEither

instance (Show e, Show a) => Show (EitherC e a) where
    showsPrec prec x = showParen (prec > 10) 
        (showString "eitherCFromEither " . showsPrec 11 (eitherCToEither x))

type LocationString = String

data Located e = Located LocationString e
    deriving (Typeable)

instance Show e => Show (Located e) where
    show (Located l e) = l ++ ":\n" ++ show e

instance Exception e => Exception (Located e)

failureWithoutLoc :: e -> EitherC e a
failureWithoutLoc e = EitherC (\ke _ -> ke e)

failureWithLoc :: LocationString -> e -> EitherC (Located e) a
failureWithLoc l e = failureWithoutLoc (Located l e)

withCurrentLoc :: Name -> ExpQ
withCurrentLoc f = appE (varE f) liftedLocationString

failure :: Q Exp
failure = withCurrentLoc 'failureWithLoc

type LErrorCall = Located ErrorCall

idLErrorCall :: EitherC LErrorCall a -> EitherC LErrorCall a
idLErrorCall = id

failureStrWithLoc :: LocationString -> String -> EitherC LErrorCall a
failureStrWithLoc l = failureWithLoc l . ErrorCall

failureStr :: Q Exp
failureStr = withCurrentLoc 'failureStrWithLoc 

data CommentedException e = CommentedException String e 
    deriving Typeable

indentStr :: String -> String
indentStr = unlines . map ("  "++) . lines

instance Show e => Show (CommentedException e) where
    show (CommentedException c e) = c++"\nInner error:\n"++indentStr (show e)

instance (Show e, Typeable e) => Exception (CommentedException e)

type LCommentedException e = Located (CommentedException e)

commentIfExceptionWithLoc
  :: LocationString
     -> String
     -> EitherC e a
     -> EitherC (LCommentedException e) a
commentIfExceptionWithLoc l comment = 
    runEitherC' (failureWithLoc l . CommentedException comment) return 

-- | Catches and rethrows with a message prepended
--
-- @$(commentIfException) :: Show e => String -> EitherC e a -> EitherC (LCommentedException e) a@
commentIfException :: Q Exp
commentIfException = [| commentIfExceptionWithLoc $liftedLocationString |]


unEitherCWithLoc :: (Show e, Typeable e) => LocationString -> String -> EitherC e c -> c
unEitherCWithLoc l comment = runEitherC' throw id . commentIfExceptionWithLoc l comment 
                
-- | Unwrap an 'EitherC', commenting the exception (if any) with the message given as first arg of the generated expression.
unEitherC :: Q Exp
unEitherC = [| unEitherCWithLoc $liftedLocationString |] 


type AttemptC = EitherC SomeException

idAttemptC :: AttemptC a -> AttemptC a
idAttemptC = id

-- | Specialization of 'unEitherCWithLoc'.
unAttemptWithLoc :: LocationString -> String -> AttemptC a -> a
unAttemptWithLoc = unEitherCWithLoc

-- | Generates a specialization of 'unEitherC'.
unAttempt :: Q Exp
unAttempt = [| unAttemptWithLoc $liftedLocationString |] 

toAttemptC :: Exception e => EitherC e a -> AttemptC a
toAttemptC = mapLeft SomeException 

-- | Comment on an exception and wrap the result into 'SomeException'
commentIfExceptionWithLoc' :: (Show e, Typeable e) =>
     LocationString -> String -> EitherC e a -> AttemptC a
commentIfExceptionWithLoc' l c = toAttemptC . commentIfExceptionWithLoc l c

-- | > $commentIfException' :: Show a1 => [Char] -> EitherC a1 a -> AttemptC a
commentIfException' :: Q Exp
commentIfException' = [| commentIfExceptionWithLoc' $liftedLocationString |]

failureWithLoc'
  :: Exception e => LocationString -> e -> AttemptC b
failureWithLoc' l = toAttemptC . failureWithLoc l

failure' :: ExpQ
failure' = withCurrentLoc 'failureWithLoc'

failureStrWithLoc' :: LocationString -> String -> AttemptC b
failureStrWithLoc' l = toAttemptC . failureStrWithLoc l

failureStr' :: ExpQ
failureStr' = withCurrentLoc 'failureStrWithLoc'

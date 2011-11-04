{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, DefaultSignatures, FlexibleInstances, FlexibleContexts, OverlappingInstances, GeneralizedNewtypeDeriving #-}
module ToPython where
import Data.Vect.Double.Base
import TupleTH
import Data.List
import Control.Monad.Writer.Lazy
import Text.Printf.TH
import Data.Functor


newtype Python a = Python { unPython :: Writer String a }
    deriving (Functor,Monad,MonadWriter String)

instance Show (Python ()) where
    show = execWriter . unPython


renderPython = execWriter . unPython

parens x = "("++x++")"
showTuple = parens . intercalate ", "


commatize :: [Python ()] -> Python ()
commatize xs = sequence_ (intersperse (py ", ") xs) 

pyTuple :: [Python ()] -> Python () 
pyTuple xs = do
    py "("
    commatize xs
    py ")"

py :: String -> Python ()
py = tell

class ToPython a where
    toPython :: a -> Python ()

    default toPython :: Show a => a -> Python ()
    toPython = tell . show

instance ToPython (Python ()) where
    toPython = id

instance ToPython ()
instance ToPython Bool
instance ToPython Int
instance ToPython Double

--instance ToPython [Char]

str :: String -> Python ()
str = py . show

instance ToPython Vec3 where
    toPython (Vec3 x y z) = toPython (x,y,z)

instance ToPython Vec4 where
    toPython (Vec4 x y z h) = toPython (x,y,z,h)

instance ToPython Mat4 where
    toPython (Mat4 a b c d) = toPython (a,b,c,d)

instance ToPython Proj4 where
    toPython = toPython . fromProjective


instance (ToPython a, ToPython b) => ToPython (a,b) where
    toPython = pyTuple . $(tupleToList 2) . $(mapTuple' 2 [|toPython|])
    
instance (ToPython a, ToPython b, ToPython c) => ToPython (a,b,c) where
    toPython = pyTuple . $(tupleToList 3) . $(mapTuple' 3 [|toPython|])

instance (ToPython a, ToPython b, ToPython c, ToPython d) => ToPython (a,b,c,d) where
    toPython = pyTuple . $(tupleToList 4) . $(mapTuple' 4 [|toPython|])

instance ToPython a => ToPython [a] where
    toPython xs = do
        py "["
        commatize (toPython <$> xs)
        py "]"

class IsTuple a
instance IsTuple ()
instance IsTuple (a,b)
instance IsTuple (a,b,c)
instance IsTuple (a,b,c,d)
instance IsTuple (a,b,c,d,e)

newtype SingleArg a = SingleArg a
instance IsTuple (SingleArg a)

instance ToPython a => ToPython (SingleArg a) where
    toPython (SingleArg a) = py "(" >> toPython a >> py ")"

methodCallExpr1 :: (ToPython obj, ToPython arg) => obj -> String -> arg -> Python ()
methodCallExpr1 obj meth arg = methodCallExpr obj meth (SingleArg arg)

methodCallExpr :: (ToPython obj, ToPython argTuple, IsTuple argTuple) => 
    obj -> String -> argTuple -> Python ()
methodCallExpr obj meth argTuple = do
    toPython (obj <.> meth)
    toPython argTuple



methodCall1 :: (ToPython obj, ToPython arg) => obj -> String -> arg -> Python ()
methodCall1 obj meth arg = methodCallExpr1 obj meth arg >> py "\n"

methodCall :: (ToPython obj, ToPython argTuple, IsTuple argTuple) => 
    obj -> String -> argTuple -> Python ()
methodCall obj meth argTuple = methodCallExpr obj meth argTuple >> py "\n"

namedArg :: ToPython r => String -> r -> Python ()
namedArg l r = assignExpr (py l) r

assignExpr :: (ToPython l, ToPython r) => l -> r -> Python ()
assignExpr l r = do
    toPython l
    py " = "
    toPython r

(.=) l r = assignExpr l r >> py "\n" 

infix 2 .=

(<.>) l r = do
    toPython l
    py "."
    py r

infixr 9 <.>

setProp :: (ToPython obj, ToPython val) => obj -> String -> val -> Python ()
setProp obj p v = obj <.> p .= v

setProp'
  :: (ToPython obj, ToPython val) => obj -> (String, val) -> Python ()
setProp' obj = uncurry (setProp obj)

-- | Add a line to the blender script
ln :: String -> Python ()
ln x = tell (x++"\n")

lns :: [String] -> Python ()
lns = mapM_ ln




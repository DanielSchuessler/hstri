{-# LANGUAGE ViewPatterns, TemplateHaskell, TemplateHaskell, NoMonomorphismRestriction, DefaultSignatures, FlexibleInstances, FlexibleContexts, OverlappingInstances, GeneralizedNewtypeDeriving #-}
module ToPython where
import Data.Vect.Double.Base
import TupleTH
import Data.List
import Control.Monad.Writer.Lazy
import Text.Printf.TH
import Data.Functor
import qualified Data.Vector as V
import Language.Haskell.TH.Syntax

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.ByteString
import Blaze.ByteString.Builder.Char.Utf8

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import qualified Data.ByteString.Lazy.UTF8(toString)

import Blaze.Text

newtype Python a = Python { unPython :: Writer Builder a }
    deriving (Functor,Monad,MonadWriter Builder)

instance Show (Python ()) where
    show = Data.ByteString.Lazy.UTF8.toString . renderPython

indent :: Python a -> Python a
indent = censor (
      fromLazyByteString 
    . BLC8.unlines 
    . fmap (BLC8.replicate 4 ' ' `mappend`) 
    . BLC8.lines 
    . toLazyByteString)
    


renderPython = toLazyByteString . execWriter . unPython

renderPythonToFile fn x = BL.writeFile fn (renderPython x)

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
py = tell . fromString

class ToPython a where
    toPython :: a -> Python ()

    default toPython :: Show a => a -> Python ()
    toPython = tell . fromShow

instance ToPython (Python ()) where
    toPython = id

instance ToPython ()
instance ToPython Bool
instance ToPython Int

instance ToPython Double where
    {-# INLINABLE toPython #-}
    toPython x =
        {-# SCC "toPython/Double" #-}
        if ({-# SCC "toPython/Double/Check" #-} isNaN x || isInfinite x)
           then error ("toPython "++show x)
           else 
            {-# SCC "toPython/Double/OK" #-}
            tell . double $ x

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
    toPython = 
        {-# SCC "toPython/2tuple" #-}
        pyTuple . $(tupleToList 2) . $(mapTuple' 2 [|toPython|])
    
instance (ToPython a, ToPython b, ToPython c) => ToPython (a,b,c) where
    toPython = 
        {-# SCC "toPython/3tuple" #-}
        pyTuple . $(tupleToList 3) . $(mapTuple' 3 [|toPython|])

instance (ToPython a, ToPython b, ToPython c, ToPython d) => ToPython (a,b,c,d) where
    toPython = 
        {-# SCC "toPython/4tuple" #-}
        pyTuple . $(tupleToList 4) . $(mapTuple' 4 [|toPython|])

instance ToPython a => ToPython [a] where
    toPython xs = 
        {-# SCC "toPython/list" #-}
        do
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


funCallExpr1 :: (ToPython arg) => String -> arg -> Python ()
funCallExpr1 meth arg = funCallExpr meth (SingleArg arg)

funCallExpr :: (ToPython argTuple, IsTuple argTuple) => 
    String -> argTuple -> Python ()
funCallExpr meth argTuple = do
    py meth
    toPython argTuple


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

setProps
  :: (ToPython obj, ToPython val) =>
     obj -> [(String, val)] -> Python ()
setProps obj = mapM_ (setProp' obj)

-- | Add a line to the blender script
ln :: String -> Python ()
ln x = py x >> tell (fromChar '\n')

lns :: [String] -> Python ()
lns = mapM_ ln



data PythonFunDef = PythonFunDef { 
    pfd_name :: String, 
    pfd_argList :: [String], 
    pfd_defBody :: Python () 
}


pfd_call1 pfd = funCallExpr1 (pfd_name pfd) 
pfd_call pfd = funCallExpr (pfd_name pfd) 

pfd_def pfd = do 
            ln ("def "++pfd_name pfd++"("++intercalate "," (pfd_argList pfd) ++"):")
            indent $ pfd_defBody pfd

instance ToPython a => ToPython (V.Vector a) where
    toPython = 
        {-# SCC "toPython/Vector" #-}
        toPython . V.toList 


foreachStmt
  :: [Char] -> Python () -> (Python () -> Python ()) -> Python ()
foreachStmt varname xs b = do
    py ("for " ++ varname ++ " in ")
    xs
    py ":\n"
    indent $ b (py varname)


-- instance Lift (Python ()) where
--     lift (Python (execWriter -> x)) = [| py x |]

instance Monoid (Python ()) where
    mempty = return ()
    mappend = (>>)

none = py "None"

emptyList ::  [()]
emptyList = [] :: [()]


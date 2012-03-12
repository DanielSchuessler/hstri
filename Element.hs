{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, NoMonomorphismRestriction, TypeFamilies, TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module Element where
import Language.Haskell.TH
import Data.Set(Set)
import HomogenousTuples
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Tuple.OneTuple
import Data.Vect.Double.Base
import Data.Vector.Unboxed(Unbox)
import Language.Haskell.TH.Build
import TupleTH
import Data.Sequence(Seq)
import qualified Data.DList as DL
import Data.DList(DList)

type family Element xs

type instance Element (OneTuple a) = a
type instance Element (a,a) = a
type instance Element (a,a) = a
type instance Element (a,a,a) = a
type instance Element (a,a,a,a) = a
type instance Element (a,a,a,a,a) = a 
type instance Element (a,a,a,a,a,a) = a 
type instance Element (Septuple a) = a
type instance Element (Dodecatuple a) = a
type instance Element [a] = a
type instance Element (Set a) = a
type instance Element (V.Vector a) = a
type instance Element (VU.Vector a) = a
type instance Element Vec2 = Double
type instance Element Vec3 = Double
type instance Element Vec4 = Double
type instance Element (e -> a) = a
type instance Element (Maybe a) = a
type instance Element (Seq a) = a
type instance Element (DList a) = a


class AsList xs where
    asList :: xs -> [Element xs]

class Mapable xs ys where
    yamap :: (Element xs -> Element ys) -> xs -> ys 

instance AsList [a] where 
    asList = id

instance AsList (V.Vector a) where
    asList = V.toList

instance (VU.Unbox a) => AsList (VU.Vector a) where
    asList = VU.toList

instance AsList (DList a) where
    asList = DL.toList

instance AsList (OneTuple a) where 
    asList = return . only

-- Make AsList instances for tuples
$(let 

        mkAsList n = 
            sequence [

                instanceD (cxt []) (''AsList `appT'` theTupleType aT)
                    [ svalD (varP 'asList) (lam1E (tupP' xs) (listE' xs)) ]

              , instanceD (cxt []) (''Mapable `appT'` theTupleType aT `appT'` theTupleType bT)
                    [ svalD (varP 'yamap) (mapTuple n) ]

              ]
            where
                xs = [ mkName ("x"++show i) | i <- [1..n] ]
                theTupleType v = Prelude.foldl appT (tupleT n) (replicate n v)
                aT = varT (mkName ("a"++show n))
                bT = varT (mkName ("b"++show n))
  in do
    decss <- mapM mkAsList ([2..6] ++ [7,12])
    return (concat decss)
    )

asListOfLists
  :: (AsList (Element a), AsList a) => a -> [[Element (Element a)]]
asListOfLists = map asList . asList


instance AsList Vec2 where asList (Vec2 a b) = [a,b]
instance AsList Vec3 where asList (Vec3 a b c) = [a,b,c]
instance AsList Vec4 where asList (Vec4 a b c d) = [a,b,c,d]




instance Mapable [x] [y] where yamap = map
instance Mapable (V.Vector x) (V.Vector y) where yamap = V.map
instance (Unbox x, Unbox y) => Mapable (VU.Vector x) (VU.Vector y) where yamap = VU.map
instance Mapable (OneTuple x) (OneTuple y) where yamap f (OneTuple x) = OneTuple (f x)
instance Mapable (e -> x) (e -> y) where yamap = fmap
instance Mapable (Maybe x) (Maybe y) where yamap = fmap
instance Mapable (Seq x) (Seq y) where yamap = fmap



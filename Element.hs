{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, TypeFamilies, TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS -Wall #-}
module Element where
import Language.Haskell.TH
import Data.Set(Set)
import HomogenousTuples
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Tuple.OneTuple
import Data.Vect.Double.Base

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


class AsList xs where
    asList :: xs -> [Element xs]

instance AsList [a] where 
    asList = id

instance AsList (V.Vector a) where
    asList = V.toList

instance (VU.Unbox a) => AsList (VU.Vector a) where
    asList = VU.toList

instance AsList (OneTuple a) where 
    asList = return . only

-- Make AsList instances for tuples
$(let 

        mkAsList n = 
            sequence [

                instanceD (cxt []) (conT ''AsList `appT` theTupleType )
                    [ valD (varP 'asList) (normalB (lam1E (tupP (fmap varP xs)) 
                                                      (listE (fmap varE xs))))
                                          []

                    ]
              ]
            where
                xs = [ mkName ("x"++show i) | i <- [1..n] ]
                theTupleType = Prelude.foldl appT (tupleT n) (replicate n aT)
                aT = varT (mkName ("a"++show n))
  in do
    decss <- mapM mkAsList ([2..6] ++ [7,12])
    return (concat decss)
    )

asListOfLists
  :: (AsList (Element a), AsList a) => a -> [[Element (Element a)]]
asListOfLists = map asList . asList

type instance Element Vec2 = Double
type instance Element Vec3 = Double
type instance Element Vec4 = Double

instance AsList Vec2 where asList (Vec2 a b) = [a,b]
instance AsList Vec3 where asList (Vec3 a b c) = [a,b,c]
instance AsList Vec4 where asList (Vec4 a b c d) = [a,b,c,d]

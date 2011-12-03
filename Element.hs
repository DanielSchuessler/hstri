{-# LANGUAGE TypeFamilies, TemplateHaskell, FlexibleInstances #-}
module Element where
import Language.Haskell.TH
import Data.Set
import HomogenousTuples

type family Element xs


type instance Element (a,a) = a
type instance Element (a,a,a) = a
type instance Element (a,a,a,a) = a
type instance Element (a,a,a,a,a) = a 
type instance Element (a,a,a,a,a,a) = a 
type instance Element (Septuple a) = a
type instance Element (Dodecatuple a) = a
type instance Element [a] = a
type instance Element (Set a) = a


class AsList xs where
    asList :: xs -> [Element xs]

instance AsList [a] where 
    asList = id

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


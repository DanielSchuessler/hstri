{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
import Simplicial.SimplicialComplex
import TypeLevel.TF.Nat
import Simplicial.DeltaSet
import Debug.Trace
import TypeLevel.TF.Nat.Small
import Simplicial.AnySimplex

main :: IO ()
--main = print (fromTris [(0::Int,1,2)])
--main = print (fromEdges [('a','b')])
main = print $ compare (AnySimplex a) (AnySimplex a)

-- fromSimplices :: forall v. Ord v => SimpsFunction (OTuple v) -> Dim -> SimplicialComplex v


a :: OTuple Char N0
a = trace "a" $ OT 'a'

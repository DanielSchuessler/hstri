{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
import Simplicial.SimplicialComplex
import TypeLevel.TF.Nat
import Simplicial.DeltaSet
import Debug.Trace
import TypeLevel.TF.Nat.Small

main :: IO ()
--main = print (fromTris [(0::Int,1,2)])
--main = print (fromEdges [('a','b')])
main = print sc

-- fromSimplices :: forall v. Ord v => SimpsFunction (OTuple v) -> Dim -> SimplicialComplex v


sc :: SimplicialComplex Char
sc = 
     mkDeltaSet 
        (\i x -> trace ("(i,x)="++show(i,x)) $ simplicialFaces i x)
        (\n -> trace ("n="++show n) $ caseNat2 n
                [a]
                [ab]
                (\_ -> []))
        (HomogenousDim 1)


a :: OTuple Char N0
a = trace "a" $ OT 'a'
ab :: OTuple Char N1
ab = trace "ab" $ OT ('a','b')

{-# LANGUAGE PatternGuards, ViewPatterns, TupleSections, NoMonomorphismRestriction #-}
module DD where

import TriangulationCxtObject
import QuadCoordinates
import qualified Data.Vector as V
import Data.Vector(Vector)
import qualified Data.Vector.Unboxed as VU
import Data.List as L
import Data.Ord
import InnerProductRepresentation
import Control.Monad.State
import Control.Applicative
import Data.Function
import Control.Exception
import VectorUtil
import PrettyUtil

data PairFate = 
    PairFate {
        pf_fst, pf_snd :: IPR,
        pf_kind :: PairFateKind
    }

instance Show PairFate where showsPrec = prettyShowsPrec

instance Pretty PairFate where
    pretty (PairFate x y z) = 
        string "P" <> 
            (parens $ hsep (punctuate comma 
                [ pretty (ipr_index x), pretty (ipr_index y), pretty z ]))


data PairFateKind =     
        Incompatible
    |   NotAdjacent IPR
    |   OK VectorIndex

    deriving(Show)

instance Pretty PairFateKind where pretty = string . show



nextIndex :: State VectorIndex VectorIndex
nextIndex = do
    i <- get
    put (succ i)
    return i

dd tr =
    let
        d = fi $ tNumberOfNormalQuadTypes tr 
        mes = sortBy (comparing (fmap (/=0))) . fmap (quad_toDenseList tr) . qMatchingEquations $ tr
        g = length mes

        mes' = transpose mes

        go = do
            _V0 <- V.zipWithM (\k meColumn ->
                        IPR 
                            <$> nextIndex 
                            <*> pure (VU.generate d (/=k))
                            <*> pure (V.fromList meColumn))

                        (V.enumFromN 0 d) (V.fromList mes')


            (_V0,) <$> loop 1 _V0

        loop i _ | i == g = return []

        loop i _Vp =
            let
                (_Sneg,_Snn ) = V.partition ((<0)  . ipr_head) _Vp
                (_S0  ,_Spos) = V.partition ((==0) . ipr_head) _Snn

                goPair :: IPR -> IPR -> State VectorIndex PairFate
                goPair x y = PairFate x y <$>
                    (case () of
                          _ | not (ipr_compatible x y) -> 
                                return Incompatible                
                            | Just z <- (disproveAdjacency _Vp x y) -> 
                                return (NotAdjacent z)
                            | otherwise ->
                                OK <$> nextIndex)

            in
                do
                    pairFates <- V.sequence 
                                    (V.concatMap 
                                        (\x -> V.map (goPair x) _Sneg)
                                        _Spos)

                    let _V = 
                            V.map ipr_tail _S0
                            V.++
                            v_mapMaybe (\pf -> 
                                case pf of
                                    PairFate x y (OK i) -> Just (ipr_combine x y i)
                                    _ -> Nothing)

                                pairFates
                    
                    (:) (i,pairFates,_V) <$> loop (i+1) _V

            

        

    in 
        evalState go 0 



ipr_compatible = zeroSetCompatible `on` zeroSet 

zeroSetCompatible z1 z2 = zeroSetAdmissible (VU.zipWith (&&) z1 z2)

zeroSetAdmissible z =
    let
        (t,t') = VU.length z `divMod` 3 
    in
        assert (t'==0) $

            all (\((*3) -> i) -> 
                atLeastTwo
                    (VU.unsafeIndex z (i))
                    (VU.unsafeIndex z (i+1))
                    (VU.unsafeIndex z (i+2)))

                [0 .. t-1]
                


atLeastTwo :: Bool -> Bool -> Bool -> Bool
atLeastTwo a b c = 
    (if a then (||) else (&&)) b c


disproveAdjacency :: Vector IPR -> IPR -> IPR -> Maybe IPR 
disproveAdjacency _Vp x y =
    let
        interxy = (VU.zipWith (&&) `on` zeroSet) x y 

        implies b1 b2 = not b1 || b2

    in
        V.find (\z -> 
            ipr_index z /= ipr_index x && 
            ipr_index z /= ipr_index y && 
            VU.and (VU.zipWith implies interxy (zeroSet z))) 

            _Vp


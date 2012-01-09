{-# LANGUAGE NamedFieldPuns, PatternGuards, ViewPatterns, TupleSections, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module VerboseDD 
    (module InnerProductRepresentation,
     dd,
     PairFate(..),
     PairFateKind(..),
     ddSolutions,
     ddSolutions',
     ppDDRes)


    where

import Control.Applicative
import Control.Exception
import Control.Monad.State
import Data.Function
import Data.List as L
import Data.Ord
import Data.Vector(Vector)
import InnerProductRepresentation
import PrettyUtil
import QuadCoordinates
import TriangulationCxtObject
import VectorUtil
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import MathUtil

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

data DDStepResult = DDSR { 
    _Sneg, _S0, _Spos :: Vector IPR, 
    pairFates :: Vector PairFate 
}

type DDResult = ([DDStepResult],Vector IPR)

ipr_init :: Int -> Int -> [Rational] -> VectorIndex -> IPR
ipr_init d k meColumn ix =
 IPR 
                            ix 
                            (VU.generate d (/=k))
                            (V.fromList meColumn)
                            (V.generate d (\j -> if j==k then 1 else 0)) 

dd
  :: Triangulation -> DDResult 
dd tr =
    let
        d = tNumberOfNormalQuadTypes tr 
        mes = sortBy (comparing (fmap (/=0))) . fmap (quad_toDenseList tr) . qMatchingEquationsRat 
                    $ tr
        g = length mes

        mes' = transpose mes

        go = do
            _V0 <- V.zipWithM (\k meColumn -> ipr_init d k meColumn <$> nextIndex)

                        (V.enumFromN 0 d) (V.fromList mes')


            loop mempty 1 _V0

        loop acc i _Vp 
            | i == g = return (reverse acc,_Vp) 
            | otherwise =

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
                                    PairFate x y (OK i_) -> Just (ipr_combine x y i_)
                                    _ -> Nothing)

                                pairFates
                    
                    let ddsr = DDSR {_Sneg, _S0, _Spos, pairFates}
                    loop (ddsr:acc) (i+1) _V

            

        

    in 
        evalState go 0 



ipr_compatible :: IPR -> IPR -> Bool
ipr_compatible = zeroSetCompatible `on` zeroSet 
    where
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


vsepVec :: Vector Doc -> Doc
vsepVec = vsep . V.toList

ppDDRes :: DDResult -> Doc
ppDDRes (steps,res) =
           vsep (zipWith ppStep [1..] steps)
        <> line <> text "Result"
        <> line <> indent 2 (ppVecs res)
        <> line <> text "IntegerResult"
        <> line <> indent 2 (ppVecs (ipr_makeIntegral <$> res))

    where
        ppStep i r =

                line <> text "Step" <+> int i
                <> line 
                <> indent 2 (let f x vs =
                                    text x <> text "^(" <> int (i-1) <> text ")" 
                                    <+> text "=" <> line
                                    <> indent 2 (ppVecs vs) <> line

                             in
                                    f "S0" (_S0 r) 
                                <>  f "S+" (_Spos r) 
                                <>  f "S-" (_Sneg r) 
                                <>  line
                                <>  indent 2 (vsepVec (pretty <$> pairFates r)))

ppVecs :: Vector IPR -> Doc
ppVecs vs =
    let 
        maxWidth = V.maximum (V.map ipr_maxScalarWidth vs)
    in 
        vsepVec (ipr_pretty BlankZeros maxWidth <$> vs)


ddSolutions :: DDResult -> Vector (Vector Rational)
ddSolutions = V.map ipr_value . snd 

ddSolutions' :: DDResult -> Vector (Vector Integer)
ddSolutions' = V.map makeVecIntegral . ddSolutions


prop_dd tr =
    let
        sols = ddSolutions' tr
    in
        all (



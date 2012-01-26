{-# LANGUAGE RecordWildCards, TemplateHaskell, NamedFieldPuns, PatternGuards, ViewPatterns, TupleSections, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module VerboseDD 
    (module InnerProductRepresentation,
     dd,
     dds,
     PairFate(..),
     PairFateKind(..),
     ddSolutions,
     ddSolutions',
     ddSolutionsToSDense,
     ddSolutionsToQDense,
     ppDDRes,
     qVertexSolutions,
     qVertexSolutionExts
     
     )


    where

import Control.Applicative
import Control.Monad.State
import Data.Function
import Data.List as L
import Data.Ord
import Data.Vector(Vector)
import InnerProductRepresentation
import MathUtil
import PrettyUtil
import QuadCoordinates
import QuadCoordinates.Dense
import TriangulationCxtObject
import Util
import VectorUtil
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import QuadCoordinates.CanonExt
--import QuadCoordinates.Class
import HomogenousTuples
import StandardCoordinates.MatchingEquations
import QuadCoordinates.MatchingEquations
import Triangulation.Class

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


data DDInput = DDInput {
    numberOfVariables :: Int,
    hyperplanes :: [[Rational]],
    compatible :: IPR -> IPR -> Bool
}

dd :: ToTriangulation t => t -> DDResult
dd (toTriangulation -> tr) = ddWith DDInput {
    numberOfVariables = tNumberOfNormalQuadTypes tr,
    hyperplanes = fmap (quad_toDenseList tr) . qMatchingEquationsRat $ tr,
    compatible = ipr_compatible
                    [ (i,i+1,i+2) | i <- map (3*) [0.. tNumberOfTetrahedra tr - 1] ]

    }

dds :: ToTriangulation t => t -> DDResult
dds (toTriangulation -> tr) = ddWith DDInput {
    numberOfVariables = tNumberOfNormalDiscTypes tr,
    hyperplanes = 
        [ map (toRational . evalMatchingEquation me) (tINormalDiscs tr)
            | me <- matchingEquationReasons tr ],


    compatible = ipr_compatible
                    [ map3 (fromEnum . iNormalQuadToINormalDisc) 
                        (normalQuads (tindex i))
                            | i <- [0.. tNumberOfTetrahedra tr - 1] ]

    }



ddWith :: DDInput -> DDResult
ddWith DDInput{..} =
    let
        mes = sortBy (comparing (fmap (/=0))) hyperplanes

        g = length mes

        mes' = transpose mes

        go = do
            _V0 <- V.zipWithM (\k meColumn -> ipr_init numberOfVariables k meColumn <$> nextIndex)

                        (V.enumFromN 0 numberOfVariables) (V.fromList mes')


            loop mempty 1 _V0

        loop acc i _Vp 
            | i == g+1 = return (reverse acc,_Vp) 
            | otherwise =

--                 $(traceExps "dd/loop" [ [|i|],[|V.length _Vp|] ]) $

            let
                (_Sneg,_Snn ) = V.partition ((<0)  . ipr_head) _Vp
                (_S0  ,_Spos) = V.partition ((==0) . ipr_head) _Snn

                goPair :: IPR -> IPR -> State VectorIndex PairFate
                goPair x y = PairFate x y <$>
                    (case () of
                          _ | not (compatible x y) -> 
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



ipr_compatible :: [Triple Int] -> IPR -> IPR -> Bool
ipr_compatible quadIndexTriples = zeroSetCompatible `on` zeroSet 
    where
        zeroSetCompatible z1 z2 = zeroSetAdmissible (VU.zipWith (&&) z1 z2)

        zeroSetAdmissible z =

                    all (\(i0,i1,i2) ->
                            atLeastTwo
                                (VU.unsafeIndex z i0)
                                (VU.unsafeIndex z i1)
                                (VU.unsafeIndex z i2))

                        quadIndexTriples
                        




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

ddSolutionsToQDense :: ToTriangulation tr => tr -> DDResult -> Vector (QAdmissible QuadDenseI)
ddSolutionsToQDense tr = V.map (either _err id . quad_admissible tr . qd_fromVector) . ddSolutions' 
    where
        _err e =
            error ("ddSolutionsToQDense: result vector not admissible: "++e)

ddSolutionsToSDense
  :: ToTriangulation tr =>
     tr -> DDResult -> Vector (Admissible (CanonExt QuadDenseI Integer))
ddSolutionsToSDense tr = V.map (canonExt tr) . ddSolutionsToQDense tr

qVertexSolutions :: ToTriangulation t => t -> Vector (QAdmissible QuadDenseI)
qVertexSolutions tr = ddSolutionsToQDense tr (dd tr) 

qVertexSolutionExts
  :: ToTriangulation t => t -> Vector (Admissible (CanonExt QuadDenseI Integer))
qVertexSolutionExts tr = ddSolutionsToSDense tr (dd tr) 


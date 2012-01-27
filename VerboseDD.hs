{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TemplateHaskell, NamedFieldPuns, PatternGuards, ViewPatterns, TupleSections, NoMonomorphismRestriction #-}
{-# LANGUAGE PolymorphicComponents, ExistentialQuantification #-}
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
import Control.Monad.State.Strict
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
import QuadCoordinates.CanonExt
--import QuadCoordinates.Class
import StandardCoordinates.MatchingEquations
import QuadCoordinates.MatchingEquations
import Triangulation.Class
import Data.BitVector.Adaptive
import Data.Proxy

data PairFate w = 
    PairFate {
        pf_fst, pf_snd :: IPR w,
        pf_kind :: PairFateKind w
    }

instance BitVector w => Show (PairFate w) where showsPrec = prettyShowsPrec

instance BitVector w => Pretty (PairFate w) where
    pretty (PairFate x y z) = 
        string "P" <> 
            (parens $ hsep (punctuate comma 
                [ pretty (ipr_index x), pretty (ipr_index y), pretty z ]))


data PairFateKind w =     
        Incompatible
    |   NotAdjacent (IPR w)
    |   OK VectorIndex

    deriving(Show)

instance BitVector w => Pretty (PairFateKind w) where pretty = string . show



nextIndex :: State VectorIndex VectorIndex
nextIndex = do
    i <- get
    put (succ i)
    return i

data DDStepResult w = DDSR { 
    _Sneg, _S0, _Spos :: Vector (IPR w), 
    pairFates :: Vector (PairFate w) 
}

data DDResult =
    forall w. BitVector w => 
        DDResult {
            _ddr_steps :: [DDStepResult w],
            _ddr_final :: Vector (IPR w)
        }

ipr_init :: BitVector w => Proxy w -> Int -> Int -> [Rational] -> VectorIndex -> (IPR w)
ipr_init _ d k meColumn ix =
    IPR
                            ix 
                            (bvAllBut d k)
                            (V.fromList meColumn)
                            (V.generate d (\j -> if j==k then 1 else 0)) 


data DDInput = DDInput {
    numberOfVariables :: Int,
    hyperplanes :: [[Rational]],
    quadIndexOffsets :: [BitVectorPosition]
}

dd :: ToTriangulation t => t -> DDResult
dd (toTriangulation -> tr) = 
    let
        d = tNumberOfNormalQuadTypes tr
    in

    withBitVectorType d ( 
        ddWith DDInput {
            numberOfVariables = d,
            hyperplanes = fmap (quad_toDenseList tr) . qMatchingEquationsRat $ tr,
            quadIndexOffsets = map (3*) [0.. tNumberOfTetrahedra tr - 1]

        }
    )

dds :: ToTriangulation t => t -> DDResult
dds (toTriangulation -> tr) = 
    let
        d = tNumberOfNormalDiscTypes tr
    in

    withBitVectorType d ( 
    ddWith DDInput {
        numberOfVariables = d,
        hyperplanes = 
            [ map (toRational . evalMatchingEquation me) (tINormalDiscs tr)
                | me <- matchingEquationReasons tr ],


        quadIndexOffsets = 
                        [ (fromEnum . iNormalQuadToINormalDisc) (tindex i ./ minBound)
                                | i <- [0.. tNumberOfTetrahedra tr - 1] ]

     })


{-# SPECIALIZE ddWith :: DDInput -> Proxy (BitVectorSingle Word) -> DDResult #-}
{-# SPECIALIZE ddWith :: DDInput -> Proxy (BitVectorSingle Word64) -> DDResult #-}
ddWith :: forall w. BitVector w => DDInput -> Proxy w -> DDResult
ddWith DDInput{..} bitvectorTypeProxy =
    let
        mes = sortBy (comparing (fmap (/=0))) hyperplanes

        g = length mes

        mes' = transpose mes

        go = do
            _V0 <- V.zipWithM 
                        (\k meColumn -> ipr_init 
                                            bitvectorTypeProxy 
                                            numberOfVariables 
                                            k 
                                            meColumn 
                                                <$> nextIndex)

                        (V.enumFromN 0 numberOfVariables) 
                        (V.fromList mes')


            loop mempty 1 _V0

        loop acc i _Vp 
            | i == g+1 = return (DDResult (reverse acc) _Vp) 
            | otherwise =

--                 $(traceExps "dd/loop" [ [|i|],[|V.length _Vp|] ]) $

            let
                (_Sneg,_S0,_Spos) = partitionBySign _Vp

                goPair :: (IPR w) -> (IPR w) -> State VectorIndex (PairFate w)
                goPair x y = {-# SCC "ddWith/goPair" #-} 

                
                        PairFate x y <$>
                            (case (bvIntersect `on` zeroSet) x y of
                                zeroSetIntersection 
                                    | not (zeroSetAdmissible quadIndexOffsets zeroSetIntersection) -> 
                                        return Incompatible                
                                    | Just z <- (disproveAdjacency _Vp x y zeroSetIntersection) -> 
                                        return (NotAdjacent z)
                                    | otherwise ->
                                        OK <$> nextIndex)

            in
                do
                    pairFates <- {-# SCC "ddWith/pairFates" #-} 
                                 V.sequence 
                                    (V.concatMap 
                                        (\x -> V.map (goPair x) _Sneg)
                                        _Spos)

                    let _V = {-# SCC "ddWith/_V" #-} 
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


partitionBySign
  :: Vector (IPR w)
     -> (Vector (IPR w), Vector (IPR w), Vector (IPR w))
partitionBySign _Vp = (_Sneg,_S0,_Spos)
    where
                (_Sneg,_Snn ) = V.partition ((<0)  . ipr_head) _Vp
                (_S0  ,_Spos) = V.partition ((==0) . ipr_head) _Snn


-- ipr_compatible :: BitVector w => [Int] -> (IPR w) -> (IPR w) -> Bool
-- ipr_compatible quadIndexOffsets = zeroSetCompatible `on` zeroSet 
--     where
--         zeroSetCompatible z1 z2 = zeroSetAdmissible (bvIntersect z1 z2)

zeroSetAdmissible
  :: BitVector w => [BitVectorPosition] -> w -> Bool
zeroSetAdmissible quadIndexOffsets z =

            all (\i ->
                    atLeastTwo
                        (bvUnsafeIndex z i)
                        (bvUnsafeIndex z (i+1))
                        (bvUnsafeIndex z (i+2))
                        
                        
                        )

                quadIndexOffsets
                        




disproveAdjacency :: BitVector w => Vector (IPR w) -> IPR w -> IPR w -> w -> Maybe (IPR w)
disproveAdjacency _Vp x y xy_zeroSetIntersection =
        V.find (\z -> 
            ipr_index z /= ipr_index x && 
            ipr_index z /= ipr_index y && 
            bvSubset xy_zeroSetIntersection (zeroSet z)) 

            _Vp


vsepVec :: Vector Doc -> Doc
vsepVec = vsep . V.toList

ppDDRes :: DDResult -> Doc
ppDDRes (DDResult steps res) =
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

ppVecs :: BitVector w => Vector (IPR w) -> Doc
ppVecs vs =
    let 
        maxWidth = V.maximum (V.map ipr_maxScalarWidth vs)
    in 
        vsepVec (ipr_pretty BlankZeros maxWidth <$> vs)


ddSolutions :: DDResult -> Vector (Vector Rational)
ddSolutions (DDResult _ x) = V.map ipr_value x

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
ddSolutionsToSDense tr = V.map canonExt . ddSolutionsToQDense tr

qVertexSolutions :: ToTriangulation t => t -> Vector (QAdmissible QuadDenseI)
qVertexSolutions tr = ddSolutionsToQDense tr (dd tr) 

qVertexSolutionExts
  :: ToTriangulation t => t -> Vector (Admissible (CanonExt QuadDenseI Integer))
qVertexSolutionExts tr = ddSolutionsToSDense tr (dd tr) 


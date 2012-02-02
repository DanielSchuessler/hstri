{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, RecordWildCards, TemplateHaskell, NamedFieldPuns, PatternGuards, ViewPatterns, TupleSections, NoMonomorphismRestriction #-}
{-# LANGUAGE PolymorphicComponents, ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS -Wall #-}
module VerboseDD 
    (module InnerProductRepresentation,
     module StandardCoordinates.MatchingEquations,
     module StandardCoordinates.Dense,
     module QuadCoordinates.CanonExt,
     module QuadCoordinates.MatchingEquations,
     module QuadCoordinates.Dense,
     dd,
     dds,
     PairFate(..),
     PairFateKind(..),
     DDableCoordSystem(..),
     ddWrapSolutions,
     ppDDRes,
     vertexSolutions,
     qVertexSolutions,
     sVertexSolutions,
     fundEdgeSolutions,
     qVertexSolutionExts,
     
     )


    where

import Control.Applicative
import Control.Monad.State.Strict
import Data.BitVector.Adaptive
import Data.Function
import Data.List as L
import Data.Ord
import Data.Proxy
import Data.Vector(Vector)
import InnerProductRepresentation
import MathUtil
import PrettyUtil
import QuadCoordinates
import QuadCoordinates.Class
import QuadCoordinates.CanonExt
import QuadCoordinates.Dense
import QuadCoordinates.MatchingEquations
import StandardCoordinates.Class
import StandardCoordinates.Dense
import StandardCoordinates.MatchingEquations
import Triangulation.Class
import Util
import VectorUtil
import qualified Data.Vector as V
import CheckAdmissibility
import Data.Ratio
import qualified Data.Vector.Generic as VG

#if 0
import Debug.Trace(trace)
#else
trace :: a -> b -> b
trace = const id
#endif




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

data DDResult c =
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



class ( CheckAdmissibility c (WrappedVector c V.Vector Rational) adm Rational
      , RatioToIntegral 
            (adm (WrappedVector c Vector Rational))
            (adm (WrappedVector c Vector Integer))
      , NonNegScalable Rational (adm (WrappedVector c Vector Rational))
      ) 

        => DDableCoordSystem c adm where

    numberOfVariables :: c -> Triangulation -> Int
    hyperplanes :: c -> Triangulation -> [[Rational]]
    quadIndexOffsets :: c -> Triangulation -> [BitVectorPosition]



instance DDableCoordSystem QuadCoordSys QAdmissible where
    numberOfVariables _ = tNumberOfNormalQuadTypes 
    hyperplanes _ tr = fmap (quad_toDenseList tr) . qMatchingEquationsRat $ tr
    quadIndexOffsets _ tr = map (3*) [0.. tNumberOfTetrahedra tr - 1]



instance DDableCoordSystem StdCoordSys Admissible where
        numberOfVariables _ = tNumberOfNormalDiscTypes
        hyperplanes _ tr = 
            [ map (toRational . evalMatchingEquation me) (tINormalDiscs tr)
                | me <- matchingEquationReasons tr ]

        quadIndexOffsets _ tr = 
                        [ (fromEnum . iNormalQuadToINormalDisc) (tindex i ./ minBound)
                                | i <- [0.. tNumberOfTetrahedra tr - 1] ]

dd :: ToTriangulation t => t -> DDResult QuadCoordSys
dd = ddWith0 (undefined :: QuadCoordSys)  

dds :: ToTriangulation t => t -> DDResult StdCoordSys
dds = ddWith0 (undefined :: StdCoordSys)  

ddWith0
  :: (ToTriangulation tr, DDableCoordSystem c adm) => c -> tr -> DDResult c
ddWith0 coord (toTriangulation -> tr) = 
    withBitVectorType 
        (numberOfVariables coord tr) 
        (ddWith coord tr) 
    


{-# SPECIALIZE ddWith :: QuadCoordSys -> Triangulation -> Proxy (BitVectorSingle Word) -> DDResult QuadCoordSys #-}
{-# SPECIALIZE ddWith :: QuadCoordSys -> Triangulation -> Proxy (BitVectorSingle Word64) -> DDResult QuadCoordSys #-}
{-# SPECIALIZE ddWith :: StdCoordSys -> Triangulation -> Proxy (BitVectorSingle Word) -> DDResult StdCoordSys #-}
{-# SPECIALIZE ddWith :: StdCoordSys -> Triangulation -> Proxy (BitVectorSingle Word64) -> DDResult StdCoordSys #-}
ddWith :: forall w c adm. (DDableCoordSystem c adm, BitVector w) => 
    c -> Triangulation -> Proxy w -> DDResult c

ddWith coordSys tr bitvectorTypeProxy =
    let
        mes = sortBy (comparing (fmap (/=0))) (hyperplanes coordSys tr)

        d = numberOfVariables coordSys tr
        g = length mes

        mes' = transpose mes

        zeroSetAdmissible_ = zeroSetAdmissible (quadIndexOffsets coordSys tr)

        go = do
            _V0 <- V.zipWithM 
                        (\k meColumn -> ipr_init 
                                            bitvectorTypeProxy 
                                            d 
                                            k 
                                            meColumn 
                                                <$> nextIndex)

                        (V.enumFromN 0 d) 
                        (V.fromList mes')


            loop mempty 1 _V0

        loop acc i _Vp 
            | i == g+1 = return (DDResult (reverse acc) _Vp) 
            | otherwise =

--                 $(traceExps "dd/loop" [ [|i|],[|V.length _Vp|] ]) $
                trace ("ddWith: length _Vp = "++show (VG.length _Vp)) $

            let
                (_Sneg,_S0,_Spos) = partitionBySign _Vp

                goPair :: (IPR w) -> (IPR w) -> State VectorIndex (PairFate w)
                goPair x y = {-# SCC "ddWith/goPair" #-} 

                
                        PairFate x y <$>
                            (case (bvIntersection `on` zeroSet) x y of
                                zeroSetIntersection 
                                    | not (zeroSetAdmissible_ zeroSetIntersection) -> 
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
        trace ("ddWith: d = "++show d++"; g = "++show g) $
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
--         zeroSetCompatible z1 z2 = zeroSetAdmissible (bvIntersection z1 z2)

zeroSetAdmissible
  :: BitVector w => [BitVectorPosition] -> w -> Bool
zeroSetAdmissible _quadIndexOffsets z =

            all (\i ->
                    atLeastTwo
                        (bvUnsafeIndex z i)
                        (bvUnsafeIndex z (i+1))
                        (bvUnsafeIndex z (i+2))
                        
                        
                        )

                _quadIndexOffsets
                        




disproveAdjacency :: BitVector w => Vector (IPR w) -> IPR w -> IPR w -> w -> Maybe (IPR w)
disproveAdjacency _Vp x y xy_zeroSetIntersection =
        V.find (\z -> 
            ipr_index z /= ipr_index x && 
            ipr_index z /= ipr_index y && 
            bvSubset xy_zeroSetIntersection (zeroSet z)) 

            _Vp


vsepVec :: Vector Doc -> Doc
vsepVec = vsep . V.toList

ppDDRes :: DDResult c -> Doc
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


ddWrapSolutions
  :: (ToTriangulation tr,
      CheckAdmissibility c (WrappedVector c Vector Rational) adm Rational) =>
     tr
     -> DDResult c -> Vector (adm (WrappedVector c Vector Rational))

ddWrapSolutions tr (DDResult _ x :: DDResult c) = 
    V.map (either _err id . admissible (undefined :: Proxy c) tr . WrappedVector . reconstruct) x

    where
        _err e =
            error ("ddWrapSolutions: result vector not admissible: "++e)


        reconstruct = ipr_value

vertexSolutions :: 
    (   DDableCoordSystem c adm
    ,   ToTriangulation tr)

    => Proxy c -> tr -> V.Vector (adm (WrappedVector c V.Vector Rational))
vertexSolutions _ tr = ddWrapSolutions tr . ddWith0 undefined $ tr 

-- | The vertices of the projective solution space in quadrilateral coordinates. 
--
-- Type restriction of 'vertexSolutions'.
qVertexSolutions :: ToTriangulation t => t -> Vector (QAdmissible QuadDenseR)
qVertexSolutions = vertexSolutions quadCoordSys

-- | The vertices of the projective solution space in standard coordinates. 
--
-- Type restriction of 'vertexSolutions'.
sVertexSolutions :: ToTriangulation t => t -> Vector (Admissible StandardDenseR)
sVertexSolutions = vertexSolutions stdCoordSys

fundEdgeSolutions
  :: ToTriangulation tr => tr -> Vector (Admissible StandardDenseI)
fundEdgeSolutions = V.map standard_toFundEdgeSol . sVertexSolutions

qVertexSolutionExts
  :: ToTriangulation tr =>
     tr -> Vector (Admissible (CanonExt QuadDenseR Rational))
qVertexSolutionExts = V.map canonExt . qVertexSolutions


{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, RecordWildCards, TemplateHaskell, NamedFieldPuns, PatternGuards, ViewPatterns, TupleSections, NoMonomorphismRestriction #-}
{-# LANGUAGE PolymorphicComponents, ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-imports #-}
module VerboseDD 
    (module VerboseDD.Types,
     module InnerProductRepresentation,
     module StandardCoordinates.MatchingEquations,
     module StandardCoordinates.Dense,
     module QuadCoordinates.CanonExt,
     module QuadCoordinates.MatchingEquations,
     module QuadCoordinates.Dense,
     module CoordSys,
     dd,
     dds,
     PairFate(..),
     PairFateKind(..),
     ddWrapSolutions,
     ppDDRes,
     vertexSolutions,
     qVertexSolutions,
     sVertexSolutions,
     fundEdgeSolutions,
     qVertexSolutionExts,
     
     )


    where

import CheckAdmissibility
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Writer.Lazy
import CoordSys
import Data.BitVector.Adaptive
import Data.Function
import Data.List as L
import Data.Ord
import Data.Proxy
import Data.Ratio
import Data.Vector(Vector)
import InnerProductRepresentation
import MathUtil
import PrettyUtil
import QuadCoordinates
import QuadCoordinates.CanonExt
import QuadCoordinates.Class
import QuadCoordinates.Dense
import QuadCoordinates.MatchingEquations
import StandardCoordinates.Dense
import StandardCoordinates.MatchingEquations
import Triangulation.Class
import Util
import VectorUtil
import VerboseDD.Types
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

#if 0
import Debug.Trace(trace)
#else
trace :: a -> b -> b
trace = const id
#endif


data DDResult co =
    forall w. BitVector w => 
        DDResult {
            _ddr_steps :: [DDStepResult (IPR co w)],
            _ddr_final :: Vector (IPR co w)
        }




ipr_init :: BitVector w => Proxy w -> Int -> Int -> [Rational] -> VectorIndex -> (IPR co w)
ipr_init _ d k meColumn ix =
    IPR
                            ix 
                            (bvAllBut d k)
                            (V.fromList meColumn)
                            (V.generate d (\j -> if j==k then 1 else 0)) 




dd :: ToTriangulation t => t -> DDResult QuadCoordSys
dd = ddWith0 quadCoordSys

dds :: ToTriangulation t => t -> DDResult StdCoordSys
dds = ddWith0 stdCoordSys

ddWith0
  :: (ToTriangulation tr, CoordSys c adm) => Proxy c -> tr -> DDResult c
ddWith0 coord (toTriangulation -> tr) = 
    withBitVectorType 
        (numberOfVariables coord tr) 
        (ddWith coord tr) 
    


{-# SPECIALIZE ddWith :: Proxy QuadCoordSys -> Triangulation -> Proxy (BitVectorSingle Word) -> DDResult QuadCoordSys #-}
{-# SPECIALIZE ddWith :: Proxy QuadCoordSys -> Triangulation -> Proxy (BitVectorSingle Word64) -> DDResult QuadCoordSys #-}
{-# SPECIALIZE ddWith :: Proxy StdCoordSys -> Triangulation -> Proxy (BitVectorSingle Word) -> DDResult StdCoordSys #-}
{-# SPECIALIZE ddWith :: Proxy StdCoordSys -> Triangulation -> Proxy (BitVectorSingle Word64) -> DDResult StdCoordSys #-}
ddWith :: forall w co adm. (CoordSys co adm, BitVector w) => 
    Proxy co -> Triangulation -> Proxy w -> DDResult co

ddWith coordSys tr bitvectorTypeProxy =
    let
        mes = sortBy (comparing (fmap (/=0))) (hyperplanes coordSys tr)

        d = numberOfVariables coordSys tr
        g = length mes

        mes' = transpose mes


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


            loop 1 _V0

        loop i _Vp 
            | i == g+1 = return _Vp 
            | otherwise =

--                 $(traceExps "dd/loop" [ [|i|],[|V.length _Vp|] ]) $
                trace ("ddWith: length _Vp = "++show (VG.length _Vp)) $

            let
                (_Sneg,_S0,_Spos) = partitionBySign ipr_head _Vp


            in
                do
                    pairFates <- {-# SCC "ddWith/pairFates" #-} 
                                 V.sequence 
                                    (V.concatMap 
                                        (\x -> V.map (goPair tr _Vp x) _Sneg)
                                        _Spos)

                    let _V = {-# SCC "ddWith/_V" #-} 
                            V.map ipr_tail _S0
                            V.++
                            vectorMapMaybe (\pf -> 
                                case pf of
                                    PairFate x y (OK i_) -> Just (ipr_combine x y i_)
                                    _ -> Nothing)

                                pairFates
                    
                    tell (return DDSR {_Sneg, _S0, _Spos, pairFates})
                    loop (i+1) _V

            

        (_final,_steps) = trace ("ddWith: d = "++show d++"; g = "++show g) $
                            runVerboseDD go
        

    in 
        DDResult _steps _final



goPair
  :: (BitVector w, CoordSys co adm) =>
     Triangulation
     -> Vector (IPR co w)
     -> IPR co w
     -> IPR co w
     -> VerboseDD v (PairFate (IPR co w))
goPair tr _Vp x y = 

    fmap (PairFate x y) $


        let zeroSetIntersection = intersectZeroSets x y
        in
            if zeroSetAdmissible tr zeroSetIntersection
               then
                    case findVectorDifferentThanTheseAndWithZeroSetAtLeast x y
                            zeroSetIntersection _Vp of


                        Just z -> return (NotAdjacent z)
                        Nothing -> OK <$> nextIndex

               else
                    return Incompatible                



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

ppVecs :: BitVector w => Vector (IPR co w) -> Doc
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
    (   CoordSys c adm
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


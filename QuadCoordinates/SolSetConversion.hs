{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ImplicitParams, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module QuadToStandard.SolSetConversion where

import qualified Data.Vector.Generic as VG
import Data.Vector(Vector)
import QuadCoordinates.CanonExt
import PrettyUtil
import Data.BitVector.Adaptive
import Triangulation
import StandardCoordinates.Dense
import StandardCoordinates.Class
import Data.AdditiveGroup
import Triangulation.VertexLink
import TriangulationCxtObject
import VectorUtil
import Data.Proxy
import Data.Foldable(foldrM)
import VerboseDD.Types
import CoordSys
import NormalSurfaceBasic
import Control.Applicative

data SolSetConversionVectorRepresentation v r w = SSCVR VectorIndex (StandardDense v r) 

instance (VG.Vector v r, Eq r, Num r , BitVector w) => 
    VerboseDDVectorRepresentation (SolSetConversionVectorRepresentation v r w) StdCoordSys w where

    ddrep_zeroSet (SSCVR _ x) = sd_zeroSet x
    ddrep_index (SSCVR i _) = i






quadToStandardSolSet tr quadSolSet =
    let t = tNumberOfTetrahedra tr
    in 
        withBitVectorType t (\(_ :: Proxy w) -> 
            let ?tr = tr
            in let
                _L_0 = VG.map (conv . canonExt) quadSolSet 

                _C_0 = bvEmpty (7 * tNumberOfTetrahedra tr) :: ZeroSet StdCoordSys w 

        
            in do 
                (_L'_m,_) <- foldrM vertexStep (_L_0, _C_0) (vertices tr) 
                return (VG.map sd_projectiveImage _L'_m))

                
                

conv = sd_fromStandardCoords ?tr

vertexStep v (_L_r1, _C_r1) =
    let
        _link = sd_map fromIntegral $ conv (vertexLinkingSurface v)

        _L_r_0 = 
            VG.map (partialCanonicalPart v) _L_r1 
            `VG.snoc` (negateV _link)


    in do
        (_L_r_n,_C_r_n) <- foldrM (ntriStep v) (_L_r_0,_C_r1)
                                     (vertexLinkingSurfaceTris v)

        return (_L_r_n `VG.snoc` _link, _C_r_n) 

ntriToIx = fromEnum . iNormalTriToINormalDisc

ntriStep v nt (_L_r_s1, _C_r_s1) =
    let
        (_Sneg,_S0,_Spos) = partitionBySign (flip triCount nt) _L_r_s1

    in do

        pairFates <-
                        V.sequence 
                        (V.concatMap 
                            (\x -> V.map (goPair tr _Vp x) _Sneg)
                            _Spos)

        let _L_r_s =
                 _0 `mappend` _Spos `mappend` pairResults
                V.map ipr_tail _S0
                V.++
                vectorMapMaybe (\pf -> 
                    case pf of
                        PairFate x y (OK i_) -> Just (ipr_combine x y i_)
                        _ -> Nothing)

                    pairFates


            _C_r_s = bvSetBit _C_r_s1 (ntriToIx nt)
        
        tell (return DDSR {_Sneg, _S0, _Spos, pairFates})


        
        return (_C_r_s, _C_r_s) 





goPair _C tr _Vp x y = 

    fmap (PairFate x y) $


        let zeroSetIntersection = intersectZeroSets x y
        in
            if zeroSetAdmissible tr zeroSetIntersection
               then
                    case findVectorDifferentThanTheseAndWithZeroSetAtLeast x y
                            (bvIntersection zeroSetIntersection _C) _Vp of


                        Just z -> return (NotAdjacent z)
                        Nothing -> OK <$> nextIndex

               else
                    return Incompatible                


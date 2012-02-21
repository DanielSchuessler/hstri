{-# LANGUAGE TemplateHaskell, TypeFamilies, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, ImplicitParams, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-imports -fno-warn-missing-signatures #-}
module QuadCoordinates.SolSetConversion(
    module CoordSys,
    module StandardCoordinates.Dense,
    module QuadCoordinates.Class,
    quadToStandardSolSet,
    quadToStandardSolSet',
    SolSetConversionVectorRepresentation(..),
    unSSCVR,
    sscvr_index,
    sscvr_value,
    SolSetConversionResultItem(..),
    SolSetConversionResult(..),
    sscr_final,
    VertexStepResult(..),
    TriStepResult(..),
    ppSSCRes  
    ) where

import CheckAdmissibility
import Control.Applicative
import Control.Monad.Writer.Class(tell)
import CoordSys
import Data.AdditiveGroup
import Data.BitVector.Adaptive
import Data.DList(DList)
import Data.DList(DList)
import Data.Foldable(foldrM)
import Data.List(foldl')
import Data.Proxy
import Data.Ratio
import Data.Vector(Vector)
import Data.VectorSpace
import HomogenousTuples
import MathUtil
import NormalSurfaceBasic
import PrettyUtil.Matrix
import QuadCoordinates.CanonExt
import QuadCoordinates.Class
import StandardCoordinates.Dense
import Triangulation
import Triangulation.Class
import Triangulation.VertexLink
import TriangulationCxtObject
import TupleTH
import Util
import VectorUtil
import VerboseDD.Types
import qualified Data.DList as DL
import qualified Data.Vector.Generic as VG
import Data.Typeable
import Control.Monad.State.Class(put)
import VerboseDD
import Control.Arrow(second)

data SolSetConversionVectorRepresentation v r w = SSCVR VectorIndex (StandardDense v r) 

unSSCVR (SSCVR _ x) = x

sscvr_index (SSCVR i _) = i

-- | = unSSCVR
sscvr_value (SSCVR _ v) = v

instance MaxColumnWidth (v r) => MaxColumnWidth (SolSetConversionVectorRepresentation v r w) where
    maxColumnWidth = maxColumnWidth . unSSCVR

deriving instance Show (StandardDense v r) => Show (SolSetConversionVectorRepresentation v r w)



instance (VG.Vector v r, Eq r, Num r , BitVector w) => 
    VerboseDDVectorRepresentation (SolSetConversionVectorRepresentation v r w) StdCoordSys w where

    ddrep_zeroSet (SSCVR _ x) = sd_zeroSet x
    ddrep_index (SSCVR i _) = i





-- deriving instance (Show r, Num r) =>
--     Show (SolSetConversionResult r)


                
conv = sd_fromStandardCoords ?tr

partialCanonicalPart' v (SSCVR _ x) = do
    i <- nextIndex
    return (SSCVR i (partialCanonicalPart v x))


ntriToIx = fromEnum . iNormalTriToINormalDisc

data TriStepResult r w = TriStepResult { 
    tsr_C_r_s1 :: w,
    tsr_pbs :: PartitioningBySign Vector (SolSetConversionVectorRepresentation Vector r w), 
    tsr_pairFates :: Vector (PairFate (SolSetConversionVectorRepresentation Vector r w)),
    tsr_tri :: INormalTri
}
    deriving Show


instance MaxColumnWidth r => MaxColumnWidth (TriStepResult r w) where
    maxColumnWidth TriStepResult{..} = maxColumnWidth tsr_pbs 


ntriStep nt (_L_r_s1, _C_r_s1) =
    let
        pbs@PartitioningBySign {_Sneg,_S0,_Spos} = partitionBySign (flip triCount nt . unSSCVR) _L_r_s1

        goPair (x, y) =
            fmap (PairFate x y) $
                let zeroSetIntersection = intersectZeroSets x y
                in
                    if zeroSetAdmissible ?tr zeroSetIntersection
                    then
                            case findVectorDifferentThanTheseAndWithZeroSetAtLeast x y
                                    (bvIntersection zeroSetIntersection (ZeroSet _C_r_s1)) _L_r_s1 of
                                Just z -> return (NotAdjacent z)
                                Nothing -> OK <$> nextIndex

                    else
                            return Incompatible                
    in do

        tsr_pairFates <- VG.mapM goPair (vectorCart _Spos _Sneg)

        let _L_r_s =
                 _S0 `mappend` _Spos `mappend` 
                 vectorMapMaybe (\pf -> 
                    case pf of
                        PairFate (SSCVR _ u) (SSCVR _ w) (OK i_) -> 
                            let
                                u_nt = triCount u nt
                                w_nt = triCount w nt
                                d = u_nt - w_nt
                            in
                                Just 
                                    (SSCVR i_
                                        ((u_nt/d) *^ w ^-^ (w_nt/d) *^ u))
                        _ -> Nothing)

                    tsr_pairFates


            _C_r_s = bvSetBit _C_r_s1 (ntriToIx nt)
        
        teller (TriStepResult { tsr_pbs = pbs, tsr_pairFates, tsr_tri = nt, tsr_C_r_s1 = _C_r_s1})


        
        return (_L_r_s, _C_r_s) 


data VertexStepResult r w = VertexStepResult {
    vsr_vertex :: TVertex,
    vsr_partials :: Vector (VectorIndex,VectorIndex),
    vsr_neglink :: VectorIndex,
    vsr_triStepResults :: DList (TriStepResult r w),
    vsr_link :: VectorIndex
}

instance MaxColumnWidth (TriStepResult r w) => MaxColumnWidth (VertexStepResult r w) where
    maxColumnWidth = maxColumnWidth . vsr_triStepResults

data SolSetConversionResult r =

    forall w. (Show w, BitVector w) => 
        SolSetConversionResult {
            sscr_inputTriangulation :: Triangulation,
            sscr_canonExtsOfInput :: Vector ( SolSetConversionVectorRepresentation Vector r w ),  
            sscr_steps :: [VertexStepResult r w],
            sscr_finalVerbose :: Vector (SolSetConversionResultItem r) 
        }

sscr_final :: SolSetConversionResult r -> Vector (Admissible (StandardDense Vector r))
sscr_final = VG.map sscri_value . sscr_finalVerbose

data SolSetConversionResultItem r = 
    SolSetConversionResultItem {
        sscri_indexOfProjectivePreimage :: VectorIndex,
        sscri_index :: VectorIndex,
        sscri_value :: Admissible (StandardDense Vector r)
    }

vertexStep v (_L_r1, _C_r1) =
    let
        _link0 = sd_map fromIntegral (conv (vertexLinkingSurface v))
    in do


    partialCanonicalParts <- VG.mapM (partialCanonicalPart' v) _L_r1 
    neglink <- flip SSCVR (negateV _link0) <$> nextIndex

    let _L_r_0 = partialCanonicalParts `VG.snoc` neglink

    let triStepComputation = foldrM ntriStep (_L_r_0,_C_r1)
                                    (vertexLinkingSurfaceTris v) 

    ((_L_r_n,_C_r_n),triSteps) <- runVerboseDDSubWriter triStepComputation

    _link <- flip SSCVR _link0 <$> nextIndex
    teller (VertexStepResult {
        vsr_vertex = v,
        vsr_partials = VG.zipWith (\x x' -> (ddrep_index x,ddrep_index x')) _L_r1 partialCanonicalParts, 
        vsr_neglink = ddrep_index neglink,
        vsr_triStepResults = triSteps,
        vsr_link = ddrep_index _link
     })
    return (_L_r_n `VG.snoc` _link, _C_r_n) 


canonExts =
    VG.mapM (\(_,q) ->
        SSCVR <$> nextIndex <*> pure (conv . canonExt $ q))

                 

quadToStandardSolSet_core quadSolSet (_ :: Proxy w) =
    let

        _C_0 = 
                foldl' bvSetBit
                    (bvEmpty (7 * tNumberOfTetrahedra ?tr)) 
                    ((fromEnum . iNormalQuadToINormalDisc) <$> tINormalQuads ?tr)
                    
                        :: ZeroSet StdCoordSys w 


        

    in do 
        -- Set VectorIndex supply to one more than the max of the input VectorIndices 
        put (succ (maximum (0 : map fst (VG.toList quadSolSet))))
        _L_0 <- canonExts quadSolSet

        (_L'_m,_) <- foldrM vertexStep (_L_0, _C_0) (vertices ?tr) 

        final <- VG.mapM (\(SSCVR i q) -> 
                    SolSetConversionResultItem i 
                        <$> nextIndex 
                        <*> pure 
                             (toAdmissible stdCoordSys ?tr .  sd_projectiveImage $ q))
                             
                         _L'_m

        return (_L_0, final )




quadToStandardSolSet
  :: (ToTriangulation tr,
      Fractional r,
      Pretty r,
      Show r,
      Eq r,
      Pretty q,
      Show q,
      Typeable r,
      QuadCoords q r) =>
     tr -> Vector (VectorIndex,QAdmissible q) -> SolSetConversionResult r
quadToStandardSolSet (toTriangulation -> tr) quadSolSet =
    let     t = 7*tNumberOfTetrahedra tr
    in let  ?tr = tr
    in withBitVectorType t (\p ->
        let ((canonExtsOfInput, final),steps) = runVerboseDD (quadToStandardSolSet_core quadSolSet p)
        in SolSetConversionResult {
                sscr_inputTriangulation = tr,
                sscr_canonExtsOfInput = canonExtsOfInput,
                sscr_steps = steps,
                sscr_finalVerbose = final
             })



-- | Convenience wrapper taking a 'DDResult'
quadToStandardSolSet'
  :: DDResult QuadCoordSys -> SolSetConversionResult Rational
quadToStandardSolSet' DDResult { _ddr_inputTriangulation = tr, _ddr_final = final } =
    quadToStandardSolSet 
        tr
        (VG.map 
            (\ipr -> (ipr_index ipr, 
                toAdmissible quadCoordSys tr (qd_fromVector $ ipr_value ipr))) 

            final)


ppSSCRes
  :: (Integral b, PrettyScalar (Ratio b), PrettyScalar b) =>
     SolSetConversionResult (Ratio b) -> Doc
ppSSCRes SolSetConversionResult {sscr_canonExtsOfInput,sscr_steps,sscr_finalVerbose} =
           line <> text "Canonical extensions of input"
        <> line <> exts
        <> line
        <> vsep (map ppVertexStep sscr_steps)
        <> line <> text "Result"
        <> line <> indent 2 (string $ prettyMatrix final')
        <> line <> text "IntegerResult"
        <> line <> indent 2 (string . prettyMatrix . VG.map makeVecIntegral $ final') 

    where
        final' =  VG.map (sd_toVector . adm_coords . sscri_value) 
                                sscr_finalVerbose 

        exts = vsep (map (\x -> 
                 text (prettyString (ddrep_index x) ++ 
                    " := (" ++ prettyVectorWithColumnWidth columnWidth (unSSCVR x)++")"))


                        (VG.toList sscr_canonExtsOfInput))

                    <> line

        columnWidth :: Int
        columnWidth = maxColumnWidth sscr_canonExtsOfInput


ppVertexStep r  =

           text "Outer loop: Vertex" <+> pretty (vsr_vertex r) <> line 
        <> indent 2 body

    where
        maxWidth = maxColumnWidth r 

        body =
               partials
            <> prettyVI (vsr_neglink r) <> text " := Negative vertex link" <> line
            <> (vsep (map (ppTriStep maxWidth) (DL.toList (vsr_triStepResults r)))) <> line
            <> prettyVI (vsr_link r) <> text " := Vertex link" <> line
            <> line

        partials = vsep (map    (\(i,i') -> 
                                    prettyVI i' <> text " := Partial canonical part of " 
                                    <> prettyVI i) 
                                (VG.toList (vsr_partials r)))  

                    <> line

ppTriStep maxWidth r =
    let f x vs =
                            text x <> text " = " <> 
                            (if VG.null vs 
                                then text "\8709" -- empty set
                                else line <>  (ppVecs maxWidth vs))
                            <> line

        PartitioningBySign{..} = tsr_pbs r

        body = 
                        f "S0" _S0
                    <>  f "S+" _Spos
                    <>  f "S-" _Sneg
                    <>  line
                    <>  indent 2 (vsep . VG.toList $ (ppPairFateBrief <$> tsr_pairFates r))

    in

        text "Normal tri" <+> pretty (tsr_tri r) <> line
        <> indent 2 body



ppVecs
  :: (Num t1, Eq t1,
      AsList (t t1),
      VG.Vector v (SolSetConversionVectorRepresentation t t1 t2),
      VG.Vector v Doc,
      PrettyScalar t1) =>
     Int -> v (SolSetConversionVectorRepresentation t t1 t2) -> Doc
ppVecs columnWidth = vcat . VG.toList . VG.map (ppSSCVR columnWidth)


ppSSCVR
  :: (Num t1, AsList (t t1), PrettyScalar t1, Eq t1) =>
     Int -> SolSetConversionVectorRepresentation t t1 t2 -> Doc
ppSSCVR columnWidth (SSCVR i x) = 
    text (prettyString i ++ " = (" ++ prettyVectorWithColumnWidth columnWidth x++")")

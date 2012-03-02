{-# LANGUAGE FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses #-}
import Codec.Rga.Parser
import VerboseDD
import QuadCoordinates.SolSetConversion
import qualified Data.Vector.Generic as VG
import Control.Exception
import Triangulation
import Data.List
import Control.DeepSeq(force)

readTrs = readRgaZip "/usr/share/regina-normal/examples/closed-or-census-large.rga"


go tr =
    let
        qDdr = dd tr
        sDdr = dds tr
        sscr = quadToStandardSolSet' qDdr
        nq = ddr_generatedVectorCount qDdr
        nqs = sscr_generatedVectorCount sscr
        ns = ddr_generatedVectorCount sDdr
        nout = case sDdr of DDResult{ddr_final} -> VG.length ddr_final 
    in
        assert (nout == case sscr of SolSetConversionResult{sscr_finalVerbose} -> 
                                        VG.length sscr_finalVerbose) $

            putStrLn 
                (intercalate "," [show (tNumberOfTetrahedra_ tr),show nq,show nqs,show ns,show nout])
        


main = do
    trs <- readTrs 
    putStrLn ("ntets,nq,nqs,ns,nout")
    mapM_ (go . snd) (force trs)


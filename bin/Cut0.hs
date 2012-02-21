{-# LANGUAGE ImplicitParams, TemplateHaskell, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# OPTIONS -Wall #-}
module Cut0 where
import HsTri
import qualified Data.Vector.Generic as VG
import qualified Data.Vector as V


-- gluings realised in the linear illustration 
gluings_cut0_linear :: [Gluing]
gluings_cut0_linear =
        [
            gluing (0./tBCD) (1./oBCD)
        ,   gluing (1./tABC) (2./oABC)
        ]

tr_cut0 :: Triangulation
tr_cut0 = mkTriangulation 3
        (
        [   gluing (0./tACD) (2./oBDA) -- G1
        ,   gluing (1./tABD) (2./oDBC) -- F1
        ,   gluing (0./tABC) (0./oABD) -- F2 
        ]
        ++gluings_cut0_linear)


ddr_cut0 :: DDResult QuadCoordSys
ddr_cut0 = dd tr_cut0

qVertSols_cut0 :: V.Vector (QuadDense V.Vector Rational)
qVertSols_cut0 = case ddr_cut0 of
                 DDResult{_ddr_final=xs} -> VG.map (qd_fromVector . ipr_value) xs 


sscr_cut0 :: SolSetConversionResult Rational
sscr_cut0 = quadToStandardSolSet' ddr_cut0


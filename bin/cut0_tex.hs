{-# LANGUAGE ImplicitParams, TemplateHaskell, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
import HsTri
import Util
import qualified Data.Vector as V
import Control.Monad
import Control.Concurrent
import Latexable.DD
import Tikz.InteractiveLayout
import qualified Data.Map as M
import Data.Map(Map)
import Tikz.Base
import Latexable
import qualified Data.Vector.Generic as VG
import Math.SparseVector
import Blender.Conversion
import Control.Arrow
import Cut0


weightedVertexLinkGraphs :: Latex
weightedVertexLinkGraphs =
    latexEnvNL "description" (

       makeFunboxCommand "1.5em" "\\small $#1$" ++ "\n" 
    ++ makeSemicolonBoxCommand ++ "\n"
    ++ concat (
     case (ddr_cut0,sscr_cut0) of 
      ( DDResult { ddr_final = finals }, 
        SolSetConversionResult { sscr_canonExtsOfInput = theCanonExts } ) -> do
        (ipr,theCanonExt) <- zip (VG.toList finals) (VG.toList theCanonExts) 
        return (
            "\\itemNL{Gewichteter Eckenlink-Graph für "
                ++mathmode (ipr_index ipr)++":}\n"++
            let ?layout = sgl
            in 
                tikzStructureGraph tr_cut0 
                    sgee { way = QuadGiven (qd_fromVector . ipr_value $ ipr) } 
                ++ "\nLösung: "++mathmode (
                        toLatex (ddrep_index theCanonExt) 
                        ++ " := " 
                        ++ op1 "canonExt" (
                               "("
                            ++ toLatex (ipr_index ipr)
                            ++")"
                        ) ++ " =\n"
                    )
                ++ "$$" ++ enfunboxenAndSemicolonEvery7th (unSSCVR theCanonExt) ++ "$$"


                ++ "\n\n\n"
                
                )
            ))


main = do
    writeFile "/h/dipl/tex/cut0QMES.tex" (latexifyQMatchingEquations "0.7em" "0.5em" tr_cut0)
    writeFile "/h/dipl/tex/cut0DD.tex" . latexifyDDResults "2.2em" $ ddr_cut0

    let ?layout = sgl
    writeFile "/h/dipl/graphs/cut0vl.tex" (tikzStructureGraph tr_cut0 sgee) 
    writeFile "/h/dipl/graphs/cut0weightedvls.tex" weightedVertexLinkGraphs

    writeFile "/h/dipl/tex/cut0SolSetConversion.tex"  . latexifySolSetConversionResult "1.5em" $ sscr_cut0

    putStrLn "Done"



editsgl = let ?outfilePrefix = "/tmp/cut0" in let ?tr = tr_cut0 in interactiveLayoutMainWith sgl

viewsgl = do
    let texfile = "/tmp/cut0.tex"
    writeFile texfile 
        (wrapTikzAsDoc 
            (let ?layout = sgl
                in tikzStructureGraph tr_cut0 sgee))
    runPdfLatexSilentS texfile
--     (runOkularAsync "/tmp/cut0.pdf")

sgee = defaultSGEE

sgl = SGL{sgl_verts =
      [0 ./ A, 0 ./ B, 0 ./ C, 0 ./ D, 1 ./ A, 1 ./ B, 1 ./ C, 1 ./ D,
       2 ./ A, 2 ./ B, 2 ./ C, 2 ./ D],
    sgl_eds = [],
    ptcToVertex0 =
      M.fromList
        [(0 ./ A, PtcToVertex{ptcToVertex_toTriple = (C, D, B)}),
         (0 ./ B, PtcToVertex{ptcToVertex_toTriple = (C, A, D)}),
         (0 ./ C, PtcToVertex{ptcToVertex_toTriple = (B, D, A)}),
         (0 ./ D, PtcToVertex{ptcToVertex_toTriple = (B, A, C)}),
         (1 ./ A, PtcToVertex{ptcToVertex_toTriple = (B, C, D)}),
         (1 ./ B, PtcToVertex{ptcToVertex_toTriple = (C, D, A)}),
         (1 ./ C, PtcToVertex{ptcToVertex_toTriple = (A, D, B)}),
         (1 ./ D, PtcToVertex{ptcToVertex_toTriple = (A, B, C)}),
         (2 ./ A, PtcToVertex{ptcToVertex_toTriple = (B, C, D)}),
         (2 ./ B, PtcToVertex{ptcToVertex_toTriple = (A, C, D)}),
         (2 ./ C, PtcToVertex{ptcToVertex_toTriple = (B, A, D)}),
         (2 ./ D, PtcToVertex{ptcToVertex_toTriple = (B, A, C)})],
    loc0 =
      M.fromList
        [
         (1 ./ C, Canvas (ptLength xc0,ptLength (y2+dy1))),
         (1 ./ D, Canvas (ptLength xc1,ptLength (y2+dy1))),
         (2 ./ C, Canvas (ptLength 0,ptLength (y2+dy1))),

         (0 ./ C, Canvas (ptLength (xc0+dx1),ptLength y2)),
         (0 ./ D, Canvas (ptLength (xc1-dx1),ptLength y2)),

         (1 ./ A, Canvas (ptLength 0,ptLength (y2-dy1))),
         (2 ./ A, Canvas (ptLength xc1,ptLength (y2-dy1))),
         (2 ./ D, Canvas (ptLength xc0,ptLength (y2-dy1))),

         (0 ./ A, a0node 1),
         (0 ./ B, a0node (-1)),
         (1 ./ B, b1node (-1)),
         (2 ./ B, b1node (1))
        ],
    edgeLoc0 = M.fromList [],

    nodeRot0 = 
            sparse_fromAssocs 
            [
                (0 ./ C, -30),
                (0 ./ D, 30),
                (1 ./ A, 180),
                (1 ./ C, 30),
                (1 ./ D, -30),
                (2 ./ A, 90),
                (2 ./ C, 0),
                (2 ./ D, -90),

                (0 ./ A, a0phi_deg-150),
                (0 ./ B, a0phi_deg+150),
                (1 ./ B, a0phi_deg+30),
                (2 ./ B, a0phi_deg-30)

            ]
    
    
    }


    where
        a0phi_deg = 180 * a0phi_rad/pi
        a0phi_rad = pi/2 --asin (dy1/a0stretch) 

        a0stretch = 50
        a0stretch' = 65
        a0centralDistFraction = 0.4
                    
        b1node_xshift k = k*a0stretch*cos a0phi_rad
        b1node_yshift k = k*a0stretch*sin a0phi_rad

        b1node k = Canvas (ptLength x, ptLength y)
            where
                x = offsetx + b1node_xshift k 
                y = offsety + b1node_yshift k 

        a0node k = Canvas (ptLength x, ptLength y)
            where
                x = offsetx + b1node_xshift k - a0stretch'*cos (a0phi_rad+pi/2)
                y = offsety + b1node_yshift k - a0stretch'*sin (a0phi_rad+pi/2)


        xc0 = -75
        xc1 = 75

        dy1 = 50
        dx1 = 23/40 * dy1
        y2 = 200
        offsetx = 140
        offsety = 200


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

tr' :: Triangulation
tr' = mkTriangulation 3
        [   gluing (0./tACD) (2./oBDA) -- G1
        ,   gluing (1./tABD) (2./oDBC) -- F1
        ,   gluing (0./tABC) (0./oABD) -- F2 


        ,   gluing (0./tBCD) (1./oBCD)
        ,   gluing (1./tABC) (2./oABC)
        ]



imm0 ::  Floating a => Tup3 a -> SolidTorusPoint a
imm0 (unitToStd3 -> Tup4 (a,b,c,d)) = STP {
        long = 2*pi*b, 
        lat = if boundaryness == 0
                 then 0
                 else 2*pi*c/boundaryness,

        boundaryness
    }

        where boundaryness = c+d

-- ACD1 -> boundary
-- ACD2 -> boundary
-- CD1 = DA2

imm1 ::  Floating a => Tup3 a -> SolidTorusPoint a
imm1 (unitToStd3 -> Tup4 (a,b,c,d)) = STP {
        long = 2*pi*(a+b), 
        lat = if boundaryness == 0
                 then 0
                 else 2*pi*c/boundaryness,
        boundaryness

    }

        where boundaryness = a+c+d


imm2 ::  Floating a => Tup3 a -> SolidTorusPoint a
imm2 (unitToStd3 -> Tup4 (a,b,c,d)) = STP {
        long = 2*pi*(a+b+d), 
        lat = if boundaryness == 0
                 then 0
                 else 2*pi*(c+d)/boundaryness,
        boundaryness
    }

        where boundaryness = a+c+d



pr' = mkPreRenderableFromTetImmersions 
        (\(unT -> i) -> Just $ GTetE res (torusCoords' major minor . ([imm0,imm1,imm2]!!fi i))) 
        ($err' . show)
        
        tr'


    where
        major = 1.5
        minor = 1


-- cam for major=1.5 torus
birdView = (1600,1600,readCam "(Vector((1.107737421989441, -3.3140807151794434, 4.970339298248291)), Euler((0.5687100887298584, 6.7056621446681675e-06, 0.32061710953712463), 'XYZ'), 0.8575560591178853)")

-- cam for major=3 torus
-- cam = readCam "(Vector((2.1319010257720947, -6.1500420570373535, 10.139385223388672)), Euler((0.5280014872550964, 2.1189225662965328e-06, 0.32922065258026123), 'XYZ'), 0.8575560591178853)"
--

frontView = (1920,1080, readCam "(Vector((0.4581758975982666, -6.223918914794922, 0.42777082324028015)), Euler((1.488981008529663, 5.444458565762034e-06, 0.07584881782531738), 'XYZ'), 0.8575560591178853)")

midView = (1920,1300, readCam "(Vector((3.9005279541015625, -4.038114547729492, 2.9004249572753906)), Euler((1.0373719930648804, 9.68077529250877e-06, 0.7680635452270508), 'XYZ'), 0.8575560591178853)")


midView2 = (1920,1300, 

 readCam "(Vector((-5.481293678283691, 1.214869499206543, 2.9004249572753906)), Euler((1.0373719930648804, 9.68077529250877e-06, 4.4942708015441895), 'XYZ'), 0.8575560591178853)"


    )
        
lamp = defaultLamp {
    lamp_eulers = eulerAnglesXYZ (70*pi/180) 0 (-pi/6)
}





hideAllBut i = pr_hide (not . flip isSubface (tindex i))

ddr :: DDResult QuadCoordSys
ddr = dd tr'

qVertSols = case ddr of
                 DDResult{_ddr_final=xs} -> VG.map (qd_fromVector . ipr_value) xs 


sscr :: SolSetConversionResult Rational
sscr = quadToStandardSolSet' ddr

res = 
--     60  
    120

go fp (w,h,_cam) =
    blenderMain JustLook 
    . setRenderRes w h
    . setRenderFilepath fp
    . setCam _cam
    . setLamps [lamp]
    . defaultScene 
    . setHelpLineN 19
    . setHelpLineThickness 0.002
    . disableHelpLines 
    . mkBlenderable pseudomanifoldStyle


mainRender = do
    forM_ 
        [
--             ("",midView)
--             ,
            ("B",midView2)
        ]
        (\(suf,view) -> do
            go ("/h/dipl/pictures/cut0torus"++suf++".png") view pr'
            forM_ [0,1,2] 
                (\i -> go ("/h/dipl/pictures/cut0torus"++suf++show i++".png") view (hideAllBut i pr')))


weightedVertexLinkGraphs :: Latex
weightedVertexLinkGraphs =
    latexEnvNL "description" (

       makeFunboxCommand "1.5em" "\\small $#1$" ++ "\n" 
    ++ makeSemicolonBoxCommand ++ "\n"
    ++ concat (
     case (ddr,sscr) of 
      ( DDResult { _ddr_final = finals }, 
        SolSetConversionResult { sscr_canonExtsOfInput = theCanonExts } ) -> do
        (ipr,theCanonExt) <- zip (VG.toList finals) (VG.toList theCanonExts) 
        return (
            "\\itemNL{Gewichteter \\eckenlink-Graph für "
                ++mathmode (ipr_index ipr)++":}\n"++
            let ?layout = sgl
            in 
                tikzStructureGraph tr' 
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
--     writeFile "/h/dipl/tex/cut0QMES.tex" (latexifyQMatchingEquations "0.7em" "0.5em" tr')
    writeFile "/h/dipl/tex/cut0DD.tex" . latexifyDDResults "2.2em" $ ddr

    let ?layout = sgl
    writeFile "/h/dipl/graphs/cut0vl.tex" (tikzStructureGraph tr' sgee) 
    writeFile "/h/dipl/graphs/cut0weightedvls.tex" weightedVertexLinkGraphs

    writeFile "/h/dipl/tex/cut0SolSetConversion.tex"  . latexifySolSetConversionResult "1.5em" $ sscr


--     putStrLn (latexifyEns tr')
--     previewAutoEns tr' defaultSGEE
    putStrLn "Done"


editsgl = let ?outfilePrefix = "/tmp/cut0" in let ?tr = tr' in interactiveLayoutMainWith sgl

viewsgl = do
    let texfile = "/tmp/cut0.tex"
    writeFile texfile 
        (wrapTikzAsDoc 
            (let ?layout = sgl
                in tikzStructureGraph tr' sgee))
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

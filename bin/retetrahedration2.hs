{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
import HsTri
import Util
import qualified Data.Vector as V
import Control.Monad
import Control.Concurrent

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

qVertSols :: V.Vector (QAdmissible QuadDenseR)
qVertSols = ddWrapSolutions ddr

sscr :: SolSetConversionResult Rational
sscr = quadToStandardSolSet tr' qVertSols 

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



main = do
--     writeFile "/h/dipl/tex/cut0QMES.tex" (latexifyQMatchingEquations "0.7em" "0.5em" tr')
    writeFile "/h/dipl/tex/cut0DD.tex" (latexifyDDResults (dd tr'))
--     putStrLn (latexifyEns tr')
--     previewAutoEns tr' defaultSGEE
    putStrLn "Done"

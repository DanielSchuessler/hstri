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


tr_linear = mkTriangulation 3 gluings_cut0_linear
p_linear = pMap tr_linear


spqwc_linear :: SPQWithCoords TVertex
spqwc_linear = (SPQWithCoords spq coords_linear glab)   
    where

        glab   g | ngDomOrCodIs (0 ./ tABC) g = "F2"
                 | ngDomOrCodIs (2 ./ tBCD) g = "F1"
                 | ngDomOrCodIs (0 ./ tACD) g = "G1"
                 | otherwise = $undef

        spq = spq_toOtherTriangulation tr_cut0 tr_linear


        p = p_linear 

        coords_linear :: TVertex -> Vec3
        coords_linear =
            $fromJst . flip lookup (map (second (3 *&))

                    [
                     ( p (0./B) , Vec3 (0.50) (-0.50) (-0.75))
                    ,( p (0./A) , Vec3 (-0.50) (0.50) (-0.75))
                    ,( p (1./A) , Vec3 (0) (-0.50) (-0.25))
                    ,( p (2./D) , Vec3 (0.50) (0) (-0.25))
                    ,( p (0./D) , Vec3 (-0.50) (0) (-0.25))
                    ,( p (0./C) , Vec3 (0) (0.50) (-0.25))
                    ]
                    )



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



getTetImm i = GTetE (text"it") res (torusCoords' major minor . ([imm0,imm1,imm2]!!fi i)) 
    where
        major = 1.5
        minor = 1

pr' = mkPreRenderableFromTetImmersions (getTetImm . unT) tr_cut0




-- cam for major=1.5 torus
birdView = (1600,1600,readCam "(Vector((1.107737421989441, -3.3140807151794434, 4.970339298248291)), Euler((0.5687100887298584, 6.7056621446681675e-06, 0.32061710953712463), 'XYZ'), 0.8575560591178853)")

-- cam for major=3 torus
-- cam = readCam "(Vector((2.1319010257720947, -6.1500420570373535, 10.139385223388672)), Euler((0.5280014872550964, 2.1189225662965328e-06, 0.32922065258026123), 'XYZ'), 0.8575560591178853)"
--


data BView = BView {
    bview_suf :: String,
    bview_w, bview_h :: Int,
    bview_cam :: Cam
}


frontView = BView "F" 1920 1080 (readCam "(Vector((0.4581758975982666, -6.223918914794922, 0.42777082324028015)), Euler((1.488981008529663, 5.444458565762034e-06, 0.07584881782531738), 'XYZ'), 0.8575560591178853)")

midView = BView "" 1920 1300 (readCam "(Vector((3.9005279541015625, -4.038114547729492, 2.9004249572753906)), Euler((1.0373719930648804, 9.68077529250877e-06, 0.7680635452270508), 'XYZ'), 0.8575560591178853)")


midView2 = BView "B" 1920 1300 ( 

 readCam "(Vector((-5.481293678283691, 1.214869499206543, 2.9004249572753906)), Euler((1.0373719930648804, 9.68077529250877e-06, 4.4942708015441895), 'XYZ'), 0.8575560591178853)"


    )

views_torus =
        [
            midView
--             ,
--             midView2
        ] 
        
lamp_torus = defaultLamp {
    lamp_eulers = eulerAnglesXYZ (70*pi/180) 0 (-pi/6)
}





hideAllBut i = pr_hide (not . flip isSubface (tindex i))


res = 

    120



go _lamp fp BView{..} =
      void
    . blenderMain DoRender 
    . setRenderRes bview_w bview_h
    . setRenderFilepath (fp bview_suf)
    . setCam bview_cam
    . setLamps [_lamp]
    . defaultScene 
    . setHelpLineN 19
    . setHelpLineThickness 0.002
    . disableHelpLines 






mainRender = do
    dont $ forM_ views_torus 
        $ \view -> 
        
            do
                let go_torus_PM fp view = go lamp_torus fp view . mkBlenderable pseudomanifoldStyle

                go_torus_PM (\suf -> "/h/dipl/pictures/cut0torus"++suf++".png") view pr'
                forM_ [0,1,2] 
                    (\i -> go_torus_PM 
                        (\suf -> "/h/dipl/pictures/cut0torus"++suf++show i++".png") 
                        view 
                        (hideAllBut i pr'))


    case sscr_cut0 of
                 SolSetConversionResult { sscr_finalVerbose = fv } ->
                    VG.forM_ 
                        fv
                        renderSol



renderSol (sol :: SolSetConversionResultItem Rational) = do

    let fundEdgeSolution = standard_toFundEdgeSol (sscri_value sol)
        sol_ix = sscri_index sol

    dont $ renderSol_torus sol_ix fundEdgeSolution 
    renderSol_linear sol_ix fundEdgeSolution 


renderSol_torus sol_ix fundEdgeSolution = do
    let
        view = midView

        pmTriTransp = Just (defaultTrans 0.15) {
                                _fresnel = 2.5,
                                _fresnel_factor = 1.25,
                                _translucency = 0.98
                            }

        nsTriTransp = Just (defaultTrans 1) {
                                _fresnel = 1.8,
                                _fresnel_factor = 1.25,
                                _translucency = 0.85
                            }

        ba1 = 
            setTrisTransp pmTriTransp
                    
            (mkBlenderable pseudomanifoldStyle (pr_mapDs gds2 pr'))

        ba2 =
            setTrisTransp nsTriTransp
                    
            (mkBlenderable normalSurfaceStyle 
                (prnsFromTetImmersions getTetImm (gds2 (toConcrete fundEdgeSolution))))

    go  lamp_torus 
        (\suf -> "/h/dipl/pictures/cut0torus"++suf++"_v"++show (toInteger sol_ix) ++".png")
        view
        (ba1 `disjointUnion` ba2)





renderSol_linear sol_ix fundEdgeSolution = 

--         when (sol_ix/=35) $ 
        do

    let


        lamp_linear = defaultLamp {
                            lamp_eulers = eulerAnglesXYZ (-0.25*pi) 0 (-pi/6)
                        }

        pmTriTransp = Just (defaultTrans 0.15) {
                                _fresnel = 2.5,
                                _fresnel_factor = 1.25,
                                _translucency = 0.96
                            }

        nsTriTransp = Just (defaultTrans 1) {
                                _fresnel = 1.8,
                                _fresnel_factor = 1.25,
                                _translucency = 0.85
                            }

        view_linear = BView "" 1920 1300 (readCam "(Vector((-0.25006288290023804, 1.668989896774292, 0.0771770179271698)), Euler((1.1997783184051514, -2.8716702217934653e-05, 3.2107629776000977), 'XYZ'), 0.8575560591178853)")

        view_lineartop = BView "" 1700 1300 (readCam "(Vector((0.0, 0.0, 0.9873046875)), Euler((-5.802452296421734e-09, -1.5250842366754114e-08, 2.3561954498291016), 'XYZ'), 0.8575560591178853)") 

        view_lineartop_lowfov = 
            BView "" 
                1700 1300 
--                 850 650
                (readCam "(Vector((0.0, 0.0, 23.016773223876953)), Euler((0.0, 0.0, 2.3561954498291016), 'XYZ'), 0.17453298961278452)")

        posOverride = flip lookup $
            case sol_ix of
                 35 -> bc0 0 1 0.7 ++ ad0 0 1 0.7 ++ sn 1 2 0.55
                 39 -> bottom 0 1 0.55
                 40 -> bottom 0 1 0.33
                 _ -> []


          where
            go i n tet0 v0 tet1 v1 r = 
                let
                    iv0 = tet0./v0
                    iv1 = tet1./v1
                in
                    ( corn i n (p_linear iv0) (p_linear iv1) 
                    , if iv0 < iv1 then r else 1-r)

            -- bottom east to bottom west
            bottom i n r = [go i n 0 A 0 B r] 

            -- south to north (on top)
            sn i n r =  [go i n 0 C 0 D r, go i n 2 D 2 A r] 

            -- west to east (on top)
            we i n r = [go i n 2 A 0 D r, go i n 2 D 0 C r]  

            -- bottom east to northeast
            ad0 i n r = [ go i n 0 A 0 D r, go i n 0 B 2 A r ] 

            -- bottom east to southeast
            ac0 i n r = [ go i n 0 A 0 C r, go i n 0 B 2 D r ] 

            -- bottom west to top east
            bc0 i n r = [ go i n 0 B 0 C r, go i n 0 B 0 D r ] 

        ba1 = 
            setTrisTransp pmTriTransp
                    
            (fromSpqwc spqwc_linear)

        ba2 =
            setTrisTransp nsTriTransp
                    
            (fromIntegerNormalSurface posOverride spqwc_linear fundEdgeSolution) 

    go  lamp_linear
        (\suf -> "/h/dipl/pictures/cut0"++suf++"_v"++show (toInteger sol_ix) ++".png")
        view_lineartop_lowfov
        (ba1 `disjointUnion` ba2)

        
                
                

        







main = do
    mainRender
    putStrLn "Done"

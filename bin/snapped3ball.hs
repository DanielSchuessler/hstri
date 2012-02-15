{-# LANGUAGE TemplateHaskell #-}
import HsTri
import FileLocation
import PreRenderable


tr = mkTriangulation 1 [gluing (0./tABC) (0./oABD)] 

tete = GTetE 100 (standardSnapped3Ball . perm . unitToStd3) 
    where
        perm (Tup4 (a,b,c,d)) = Tup4 (c,d,a,b) 

pren = mkPreRenderableFromTetImmersions (const (Just tete)) $undef tr

cam = readCam "(Vector((1.573462963104248, 2.0371408462524414, 0.5036145448684692)), Euler((1.404733419418335, 1.1177638043591287e-05, 2.4831230640411377), 'XYZ'), 0.8575560591178853)"

lamp = LampObj {
    lamp_lamp = Sun { 
        lamp_name = "TheLamp"
    },
    lamp_location = Vec3 (-5) (-10) 8,
    lamp_eulers = eulerAnglesXYZ (0.25*pi) 0 (70*pi/180)
}


main = testBlender (setLamps [lamp] . setRenderFilepath "/h/dipl/pictures/snapped3ball.png" . setRenderRes 1200 1200 . setCam cam . defaultScene . mkBlenderable pseudomanifoldStyle $ pren)

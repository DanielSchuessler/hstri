{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad
import Diagrams.Backend.Cairo
import Diagrams.Combinators
import Diagrams.TwoD.Text
import Diagrams.TwoD.Types
import Diagrams.Util
import Graphics.Rendering.Cairo hiding(translate)
import Graphics.Rendering.Diagrams
import System.FilePath
import Data.Colour.Names
import Diagrams.Prelude

mkLetterDiagram c =
    lc red (fc green (fontSize 100 . font "DejaVu Serif" $ text [c]))
                `atop` rect 100 100
    
    
--     let r = 50 
--     in translate (-r,-r) (strut (r,r))

dir = "/h/diplomarbeit/hstri/textures"

main =
    forM_ ['A']
        (\b -> fst $ renderDia Cairo (CairoOptions (dir</>b:".png") (PNG (512,512)))
                (mkLetterDiagram b))

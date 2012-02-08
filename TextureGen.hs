{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

import Control.Monad
import Diagrams.Backend.Cairo
import Diagrams.Combinators
import Diagrams.TwoD.Text
import Diagrams.TwoD.Types
import Diagrams.Util
import Graphics.Rendering.Diagrams
import System.FilePath
import Data.Colour.Names
import Diagrams.Prelude
import Util
import Data.Colour(withOpacity)
import Data.Colour(AlphaColour)

mkLetterDiagram
  :: (Renderable Text b, Renderable (Path R2) b) =>
     Char -> AnnDiagram b R2 Any
mkLetterDiagram c =
    lc red (fc green (fontSize 100 . font "DejaVu Serif" $ text [c]))
                `atop` rect 100 100
    
    
--     let r = 50 
--     in translate (-r,-r) (strut (r,r))

dir = "/h/diplomarbeit/hstri/textures"

finishName name = dir</>name++".png"

renderAndWrite
  :: Monoid m => [Char] -> AnnDiagram Cairo R2 m -> IO ()
renderAndWrite name dia = 
    fst $ renderDia Cairo (CairoOptions (finishName name) (PNG (outputPixels,outputPixels))) dia

outputPixels = fromIntegral 4096

-- render, write, view
rwv name dia = do
    renderAndWrite name dia
    rawSystemAsyncS "gwenview" [ finishName name ]



triangulatedTriangle n = scale (1/n) . stroke $
    -- horizontals
    mconcat [ P (0,i) ~~ P (n-i,i) | i <- [ 1 .. n-1 ] ]
    <>
    -- verticals
    mconcat [ P (i,0) ~~ P (i,n-i) | i <- [ 1 .. n-1 ] ]
    <>
    -- diagonals
    mconcat [ P (i,0) ~~ P (0,i)   | i <- [ 1 .. n-1   ] ]
    <>
    -- boundary
    (close . fromVertices) [ origin, P (n,0), P (0,n) ]


theActualUnitSquare = translate (0.5,0.5) unitSquare

tt :: Double -> Diagram Cairo R2
tt n =
    ( 
      (triangulatedTriangle n # lc white # lineWidth (1/1024)) 
      <>
      (theActualUnitSquare # fc black # lc black  )
    )

  where
--     bg :: AlphaColour Double
--     bg = withOpacity white 0

main = rwv "TriTri20" (tt 20) 
    

textureGen_test = forM_ ['A'] (\b -> renderAndWrite [b] (mkLetterDiagram b))

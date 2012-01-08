{-# LANGUAGE QuasiQuotes #-}
import ParseRga
import Quote
import Control.Monad
import Data.List
import Data.String.Interpolation
import Triangulation
import Data.Binary

main =
    go "/usr/share/regina-normal/examples/closed-or-census.rga" "ClosedOrCensus6" "closedOrCensus6"
         ((<= 6). tNumberOfTetrahedra_ . snd)

go rga modname lowername predi = do

    putStrLn [str|
module $modname$ where
import Triangulation
import Data.Binary
import qualified Data.ByteString.Lazy.Char8

|]
    trs0 <- readRgaZip rga

    let trs = filter predi trs0 
    let nam i = "tr_"++show i

    units <- zipWithM (\i (l,tr) ->
            putStrLn [str|
-- | $l$
$nam i$ :: LabelledTriangulation
$nam i$ = ($quote l$, decode $quotePrec 11 (encode tr)$)

|])

                   [0..]
                   trs

    putStrLn [str|
$lowername$ :: [LabelledTriangulation]
$lowername$ =
  [ # i in [0..length units-1]:$nam i$|
  , #
  ]

-- vim: nowrap
|]



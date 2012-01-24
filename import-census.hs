{-# LANGUAGE QuasiQuotes #-}
import Codec.Rga.Parser
import Quote
import Control.Monad
import Data.List
import Data.String.Interpolation
import Triangulation
import Data.Binary

maxTets n = ((<= n). tNumberOfTetrahedra_ . snd)

main = do
    --go "/usr/share/regina-normal/examples/closed-or-census.rga" "ClosedOrCensus6" "closedOrCensus6" (maxTets 6) 
    go "/usr/share/regina-normal/examples/closed-nor-census.rga" "ClosedNorCensus8" "closedNorCensus8" (maxTets 8)

go rga modname lowername predi = do
    let fn = "/tmp/"++modname++".hs"
    putStrLn fn

    trs0 <- readRgaZip rga

    let trs = filter predi trs0 
    let nam i = "tr_"++show i

    let
     src =
            [str|
                    module $modname$ where
                    import Triangulation
                    import Data.Binary
                    import qualified Data.ByteString.Lazy.Char8

            |]
            ++
            concat (zipWith (\i (l,tr) ->
                [str|
                    -- | $l$
                    $nam i$ :: LabelledTriangulation
                    $nam i$ = ($quote l$, decode $quotePrec 11 (encode tr)$)

                |])

                   [0..]
                   trs)

            ++

            [str|
                $lowername$ :: [LabelledTriangulation]
                $lowername$ =
                    [ # i in [0..length trs-1]:$nam i$|
                    , #
                    ]

                -- vim: nowrap
            |]


    writeFile fn src

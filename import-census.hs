{-# LANGUAGE QuasiQuotes #-}
import Codec.Rga.Parser
import Quote
import Control.Monad
import Data.List
import Data.String.Interpolation
import Triangulation
import Data.Binary
import Control.Applicative
import Data.Ord

tetsp p = p . tNumberOfTetrahedra_ . snd
maxTets n = filter (tetsp (<= n))

main = do
    --go "/usr/share/regina-normal/examples/closed-or-census.rga" "ClosedOrCensus6" "closedOrCensus6" (maxTets 6) 
    --go "/usr/share/regina-normal/examples/closed-nor-census.rga" "ClosedNorCensus8" "closedNorCensus8" (maxTets 8)
    go "/usr/share/regina-normal/examples/closed-hyp-census.rga" "Bench.Triangulations" "trs" 
        (take 10 . filter (tetsp (liftM2 (&&) (>= 11) (<= 16))))
    --go "/usr/share/regina-normal/examples/closed-hyp-census.rga" "ClosedHypCensus" "ClosedHypCensus" id

go rga modname lowername filter_ = do
    let fn = "/tmp/"++modname++".hs"
    putStrLn fn

    trs0 <- readRgaZip rga

    let trs = filter_ trs0 
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


-- vim: wrap

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
import Util
import Codec.Compression.GZip

tetsp p = p . tNumberOfTetrahedra_ . snd
maxTets n = filter (tetsp (<= n))

main = do
    dont $ go "/usr/share/regina-normal/examples/closed-or-census.rga" "ClosedOrCensus6" "closedOrCensus6" (maxTets 6) 
    go "/usr/share/regina-normal/examples/closed-or-census-large.rga" "ClosedOrCensus7To10" "closedOrCensus7To10" (filter (tetsp (> 6)))
    dont $ go "/usr/share/regina-normal/examples/closed-nor-census.rga" "ClosedNorCensus8" "closedNorCensus8" (maxTets 8)
    dont $ go "/usr/share/regina-normal/examples/closed-hyp-census.rga" "Bench.Triangulations" "trs" 
        (take 10 . filter (tetsp (liftM2 (&&) (>= 11) (<= 16))))
    dont $ go "/usr/share/regina-normal/examples/closed-hyp-census.rga" "ClosedHypCensus" "ClosedHypCensus" id

go rga modname lowername filter_ = do
    let fn = "/tmp/"++modname++".hs"
    putStrLn fn

    trs0 <- readRgaZip rga

    let trs = filter_ trs0 
        it =
            quotePrec 11 
            . compressWith defaultCompressParams { compressLevel = bestCompression }
            $ encode trs

    let
     src =
            [str|
                    module $modname$ where
                    import Triangulation
                    import Data.Binary
                    import qualified Data.ByteString.Lazy.Char8
                    import Codec.Compression.GZip(decompress)

                    $lowername$ :: [LabelledTriangulation]
                    $lowername$ = decode (decompress $it$)

                    -- vim: nowrap
            |]


    writeFile fn src


-- vim: wrap

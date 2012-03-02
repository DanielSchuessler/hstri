{-# LANGUAGE NoMonomorphismRestriction, QuasiQuotes, FlexibleContexts #-}
import DataDeps
import VerboseDD
import qualified Data.Vector.Generic as VG
import Util
import qualified Data.Vector as V
import Data.String.Interpolation
import ClosedOrCensus7To10
import Data.List

cumulativeDepFun :: (CoordSys co) => DDResult co -> V.Vector Int
cumulativeDepFun DDResult { ddr_final = final } =
    VG.map (fi . ddrep_index) final


fn ext = "/tmp/dataDeps2." ++ ext
fn_dat = fn "dat"
fn_gnuplot = fn "gnuplot"
fn_pdf = fn "pdf"

vUnlines :: VG.Vector v [Char] => v [Char] -> [Char]
vUnlines = VG.foldl' (\r x -> r ++ x ++ "\n") ""

go thedata = do
    putStrLn "Writing datafile..."
    writeFile fn_dat thedata
    putStrLn "Wrote datafile"
    writeFile fn_gnuplot 
            [str|
                set terminal pdf
                set output '$fn_pdf$'
                ##set style fill transparent solid 0.1 noborder
                plot '$fn_dat$' using 1:2 with lines linewidth 0.1 linecolor "black"
                ##plot '$fn_dat$' using 1:2:(0):(0):(0):(10) with rgbalpha
            |]
    rawSystemS "gnuplot" [fn_gnuplot]
    rawSystemS "pdf-viewer" [fn_pdf]
    putStrLn "Done"




main = 
    go
        (intercalate "\n\n"
            (map 
                (vUnlines . VG.imap (\i x -> show i ++ " " ++ show x) . cumulativeDepFun . dd)
                (take 5000 closedOrCensus7To10)))



{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, ImplicitParams #-}
import HsTri
import Tikz.StructureGraph
import Tikz.StructureGraphLayout
import Tikz.Gen
import Tikz.Preview
import System.IO
import Data.Char
import Control.Applicative
import Safe


fileBase = "/tmp/graph-layout-interactive"
texfile = fileBase ++ ".tex"
pdffile = fileBase ++ ".pdf"

main = 
    let ?tr = snd tr_6
    in
        do
            sgl <- auto ?tr (tIVertices ?tr)
            let 
                sgl0 = sglToSgl0 (tIVertices ?tr) sgl
                sgee = defaultSGEE
            reRender sgl0 sgee
            runOkularAsync pdffile

            hSetBuffering stdin NoBuffering
            hSetBuffering stdout NoBuffering
            loop sgl0 defaultSGEE


loop sgl0 sgee = do
    v <- inputIVertexWithPrompt
    print v
    g <- inputS3WithPrompt

    let sgl0' = adjustPtcToVertex sgl0 v (*. g) 
        sgee' = sgee

    reRender sgl0' sgee'
    loop sgl0' sgee'

inputS3WithPrompt = do
    putStr "Enter permutation (a,w,d: transpositions; r,f: rotations): "
    inputS3

inputS3 = do
    c <- getChar
    case c of
         'a' -> return S3cba 
         'w' -> return S3acb 
         'd' -> return S3bac 
         'r' -> return S3cab
         'f' -> return S3bca
         _ -> do
             beep
             inputS3




inputIVertexWithPrompt = do
    putStr "Enter vertex: "
    inputIVertex <* putLn

inputIVertex = do
    v <- inputVertex
    i <- inputTIndex
    return (i./v)

inputVertex = do
    c <- getChar
    if c < 'a' || c > 'd' 
       then do
           beep
           inputVertex
       else return (toEnum (ord c - ord 'a') :: Vertex)

inputTIndex = do
    ((readMay . (:[])) -> mi) <- getChar

    let err = do
        beep
        inputTIndex

    case mi of
        Nothing -> err
        Just i -> if i >= tNumberOfTetrahedra_ ?tr
                    then do
                        beep
                        inputTIndex
                    else return (fromIntegral i :: TIndex)



beep = do
    putStr "beep "
    putChar '\a'

reRender sgl0 sgee = do
            writeFile texfile 
                (wrapTikzAsDoc 
                    (let ?layout = sgl0ToSgl sgl0
                     in tikzStructureGraph ?tr sgee))
            runPdfLatexSilent texfile


putLn = putStrLn ""

{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, ViewPatterns, ScopedTypeVariables, ImplicitParams #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Tikz.InteractiveLayout(interactiveLayoutMain) where

import Tikz.StructureGraphLayout
import Tikz.Gen
import Tikz.Preview
import System.IO
import Data.Char
import Control.Applicative
import Safe
import Triangulation.PreTriangulation
import Latexable
import Math.Group
import Math.Groups.S3
import Tetrahedron
import Triangulation
import Control.Monad.Cont
import Util
import Data.Void
import Triangulation.Class


texfile = ?outfilePrefix ++ ".tex"
pdffile = ?outfilePrefix ++ ".pdf"

interactiveLayoutMain
  :: ToTriangulation t => t -> FilePath -> IO ()
interactiveLayoutMain (toTriangulation -> tr) (outfilePrefix :: FilePath) = 
    do
        let ?tr = tr
        let ?outfilePrefix = outfilePrefix

        putStrLn "Computing initial layout with Graphviz..."
        sgl0 <- auto ?tr (tIVertices ?tr) (edges tr)
        let 
            sgee = defaultSGEE
        reRender sgl0 sgee
        runOkularAsync pdffile

        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering

        putStrLn "Vertex input format: [a-d][0-9]"
        putStrLn "Permutation input format: a,w,d: transpositions; r,f: rotations"
        putStrLn "Press Q at any time to quit"

        runContT (contMain sgl0) return


contMain
  :: (?outfilePrefix::FilePath, ?tr::Triangulation) =>
     StructureGraphLayout -> ContT r IO ()
contMain sgl0 = callCC (\cc -> let ?quit = cc () in loop sgl0 defaultSGEE)

type M r a = (?quit:: ContT r IO Void, ?outfilePrefix :: FilePath, ?tr :: Triangulation) => ContT r IO a

loop :: StructureGraphLayout -> StructureGraphExtraOptions -> M r ()
loop sgl0 sgee = do
    v <- inputIVertexWithPrompt
    g <- inputS3WithPrompt
    io $ putStrLn "Regenerating...\n"

    let sgl0' = adjustPtcToVertex sgl0 v (*. g) 
        sgee' = sgee

    reRender sgl0' sgee'
    loop sgl0' sgee'

inputS3WithPrompt :: M r S3
inputS3WithPrompt = do
    io $ putStr "Permutation? "
    inputS3 <* putLn


getChar' = do
    c <- io getChar
    case c of
         'Q' -> vacuous ?quit
         _ -> return c

inputS3 = do
    c <- getChar'
    case c of
         'a' -> return S3cba 
         'w' -> return S3acb 
         'd' -> return S3bac 
         'r' -> return S3cab
         'f' -> return S3bca
         _ -> do
             beep
             inputS3



inputIVertexWithPrompt :: M r IVertex
inputIVertexWithPrompt = do
    io $ putStr "Vertex? "
    inputIVertex <* putLn

inputIVertex = do
    v <- inputVertex
    i <- inputTIndex
    return (i./v)

inputVertex :: M r Vertex
inputVertex = do
    c <- getChar'
    if c < 'a' || c > 'd' 
       then do
           beep
           inputVertex
       else return (toEnum (ord c - ord 'a') :: Vertex)

inputTIndex :: M r TIndex
inputTIndex = do
    ((readMay . (:[])) -> mi) <- getChar'

    let err = do
        beep
        inputTIndex

    case mi of
        Nothing -> err
        Just i -> if i >= numberOfTetrahedra_ ?tr
                    then do
                        beep
                        inputTIndex
                    else return (fromIntegral i :: TIndex)



beep = io $ do
    putStr "beep "
    putChar '\a'

reRender sgl0 sgee = io $ do
            writeFile texfile 
                (wrapTikzAsDoc 
                    (let ?layout = sgl0
                     in tikzStructureGraph ?tr sgee))
            runPdfLatexSilentS texfile


putLn = io $ putStrLn ""

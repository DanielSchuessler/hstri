{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes, NoMonomorphismRestriction, ViewPatterns, ScopedTypeVariables, ImplicitParams #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds #-}
module Tikz.InteractiveLayout(
    interactiveLayoutMain,interactiveLayoutMainV,interactiveLayoutMainWith,
    
    ) where

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
import Control.Monad.State.Strict
import Data.Lens.Template
import Data.Lens.Strict
import Text.Groom

data TheState = TheState {
    sgl :: StructureGraphLayout,
    sgee :: StructureGraphExtraOptions
}
nameMakeLens ''TheState (Just . (++"L"))

type M0 r a = StateT TheState (ContT r IO) a
type M r a = (?quit:: M0 r Void, ?outfilePrefix :: FilePath, ?tr :: Triangulation) => M0 r a 

texfile = ?outfilePrefix ++ ".tex"
pdffile = ?outfilePrefix ++ ".pdf"

interactiveLayoutMainV
  :: ToTriangulation t => t -> FilePath -> IO ()
interactiveLayoutMainV = interactiveLayoutMain True False

interactiveLayoutMain
  :: ToTriangulation t => 
       Bool -- ^ vertices? 
    -> Bool -- ^ edges? 
    -> t -> FilePath -> IO ()
interactiveLayoutMain doVerts doEds (toTriangulation -> tr) (outfilePrefix :: FilePath) = 
    do
        let ?tr = tr
        let ?outfilePrefix = outfilePrefix

        putStrLn "Computing initial layout with Graphviz..."
        sgl0 <- auto ?tr 
                    (guard doVerts >> tIVertices tr) 
                    (guard doEds >> edges tr) 

        interactiveLayoutMainWith sgl0


interactiveLayoutMainWith sgl0 = do
        let sgee = defaultSGEE

        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering

        putStrLn "Vertex input format: [a-d][0-9]"
        putStrLn "Permutation input format: a,w,d: transpositions; r,f: rotations"
        putStrLn "Press Q at any time to quit"

        st <- runContT (execStateT contMain (TheState sgl0 sgee)) return

        writeFile (?outfilePrefix++".hs") ("sgl = "++groom (sgl st))


contMain
  :: (?outfilePrefix::FilePath, ?tr::Triangulation) =>
     M0 r ()
contMain = callCC (\cc -> do
    reRender
    io (runOkularAsync pdffile)
    let ?quit = cc () in loop)


loop :: M r b 
loop = do
    v <- inputIVertexWithPrompt
    g <- inputS3WithPrompt
    io $ putStrLn "Regenerating...\n"

    void (sglL !%= (\sgl -> adjustPtcToVertex sgl v (*. g))) 

    reRender
    loop

inputS3WithPrompt :: M r S3
inputS3WithPrompt = do
    io $ putStr "Permutation? "
    inputS3 <* putLn


getChar' = do
    c <- io getChar
    case c of
         'Q' -> do
            vacuous ?quit
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

reRender = do
    TheState {sgl, sgee} <- get 
    io $ writeFile texfile 
        (wrapTikzAsDoc 
            (let ?layout = sgl
                in tikzStructureGraph ?tr sgee))
    io $ runPdfLatexSilentS texfile


putLn = io $ putStrLn ""

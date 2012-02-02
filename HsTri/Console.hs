{-# OPTIONS -Wall #-}
module HsTri.Console where

import System.IO
import Control.Monad.Cont
import VerboseDD
import qualified Data.Vector as V
import SimplicialPartialQuotient
import Blender
import Data.Lens.Common
import ShortShow
import PrettyUtil

viewFundEdgeSolutionsInBlender
  :: (Ord v, Show v, Pretty v, ShortShow v) =>
     SPQWithCoords v
     -> (Admissible StandardDenseI
         -> Maybe
              (AnySimplex2Of
                 (DJSCons
                    (Asc3 v)
                    (Asc3 (Corn v))
                    (DJSCons (Asc2 v) (Asc2 (Corn v)) (DJSCons v (Corn v) SCMinus1)))))
     -> IO ()
viewFundEdgeSolutionsInBlender spqwc initialSelection = do
  hSetBuffering stdin NoBuffering
     

  flip runContT return $

    callCC (\quit -> do
        let sols = fundEdgeSolutions (spqwc_tr spqwc)
        V.forM_ (V.indexed sols)

            (\(i,s) -> do

                liftIO (putStrLn (show i ++ " of "++show (V.length sols)))

                _ <- liftIO 
                    . testBlender 
                    . (scene_initialSelectionL ^= initialSelection s)
                    . defaultScene 
                        $
                        
                        (fromSpqwcAndIntegerNormalSurface
                            spqwc
                            s)

                liftIO $ putStrLn "Press q to quit, any other key for next"

                c <- liftIO getChar
                when (c=='q') (quit ()))

        liftIO $ putStrLn "That's all.")
                


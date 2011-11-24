{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, ViewPatterns, TemplateHaskell, Arrows #-}    
module ParseRga where

import Control.Monad
import Data.Functor
import Data.List(sort)
import Data.Map as Map hiding(mapMaybe)
import Data.Maybe
import HomogenousTuples
--import Text.XML.Light hiding(parseXMLDoc,strContent)
import Triangulation
import TupleTH
import Text.XML.HXT.Core
import Control.Exception
import Data.Typeable
import Data.List.Split
import qualified Debug.Trace(trace)

readRgaFile fn =
    runX (readDocument opts fn >>> parseRga)
  where
    opts = [ withTrace 0 ]


parseRga =      getChildren 
            >>> getChildren
            >>> hasName "packet" 
            >>> hasAttrValue "type" (=="Triangulation")
            >>> getChildren
            >>> hasName "tetrahedra"
            >>> proc x -> do
                    n <- getRequiredAttrValue "ntet" -< x 
                    tets <- listA (arr (fmap read . words) <<< getText <<< getChildren 
                                    <<< isElem <<< getChildren) -< x
                    returnA -< translateGluings (read n) tets

getRequiredAttrValue a =
    getAttrValue0 a 
    `orElse`
    constA (throw (RequiredAttributeNotPresentException a))

translateGluings :: Int -> [[Int]] -> Triangulation
translateGluings ntet gluingRows = 
    assert (ntet == length gluingRows) $
        either error id res
    where
        res =
                mkTriangulation 
                    (fromIntegral <$> tindices)
                    (concatMap translateGluingForTet (zip tindices gluingRows))


        tetIxInBounds tet = tet >= 0 && tet < ntet

        tindices = [0..ntet-1]


        translateGluingForTet (tet, gluingRow) =
            assert (length (gluingRow) == 8) $
            assert (tetIxInBounds tet) $
            mapMaybe translateGluing (zip [0..3] (chunk 2 gluingRow))

            where

                translateGluing (_, [-1,-1]) = Nothing
                translateGluing (faceIx, [tet',dcba]) = 
                    assert (faceIx >= 0 && faceIx < 4) $
                    assert (tetIxInBounds tet') $
                    let
                        divMod4 = (`divMod` 4)
                        (dcb,a) = divMod4 dcba
                        (dc,b) = divMod4 dcb
                        (d,c) = divMod4 dc

                        vis :: Triple Int
                        vis = $(deleteAtTuple 4) faceIx (0,1,2,3)

                        vs,us :: Triple Vertex
                        vs = map3 toEnum vis

                        us = map3 (toEnum . ([a,b,c,d] !!)) vis

                        res :: Gluing
                        res =
                                (fromIntegral tet ./ triangle vs, 
                                 fromIntegral tet' ./ otriangle us)

                    in
                       -- Debug.Trace.trace (show res) 
                        (Just res)
            

data RequiredAttributeNotPresentException = RequiredAttributeNotPresentException String
    deriving (Show,Typeable)

instance Exception RequiredAttributeNotPresentException




-- g ::  (Ord t2, Read t2) => Element -> (t2, t2, t2, t2)
-- g = fromList4 . sort . fmap read . words . strContent

    

test = readRgaFile "/h/dev/regina-things/sfsHakenExample2Triang.rga"


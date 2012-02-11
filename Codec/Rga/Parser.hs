{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DeriveDataTypeable, NoMonomorphismRestriction, ViewPatterns, Arrows #-}    
{-# OPTIONS -Wall #-}
-- | Description: Parser for /Regina/ data files.
--
-- Parser for /Regina/ data files. Currently, only parsing of triangulations is supported.
module Codec.Rga.Parser(readRgaFile,readRgaZip,RequiredAttributeNotPresentException(..)) where

import Codec.Compression.GZip(decompress)
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy(readFile)
import Data.ByteString.Lazy.UTF8(toString)
import Data.List.Split
import Data.Maybe
import Data.Typeable
import HomogenousTuples
import Prelude hiding(readFile)
import Text.XML.HXT.Core as HXT
import Triangulation
import Util(fi)
import Codec.Rga.Util


readRgaZip :: FilePath -> IO [LabelledTriangulation]
readRgaZip fn = do
    inp <- readFile fn
    runX (readString syscfg (toString . decompress $ inp) >>> parseRga)

readRgaFile :: String -> IO [LabelledTriangulation]
readRgaFile fn =
    runX (readDocument syscfg fn >>> parseRga)

syscfg :: [SysConfig]
syscfg = [ withTrace 0 ]


parseRga
  :: ArrowXml cat =>
     cat XmlTree LabelledTriangulation
parseRga =      deep (hasAttrValue "type" (=="Triangulation"))
            >>> parseTriangulationPacket

parseTriangulationPacket
  :: ArrowXml t => t XmlTree LabelledTriangulation
parseTriangulationPacket = proc x -> do
    lbl <- getRequiredAttrValue "label" -< x
    tr <- (     getChildren
            >>> hasName "tetrahedra"
            >>> parseTetrahedraTag
          ) -< x
    returnA -< labelledTriangulation lbl tr
        
parseTetrahedraTag :: ArrowXml t => t XmlTree Triangulation
parseTetrahedraTag = proc x -> do
                    n <- getRequiredAttrValue "ntet" -< x 
                    tets <- listA (arr (fmap read . words) <<< getText <<< getChildren 
                                    <<< isElem <<< getChildren) -< x
                    returnA -< translateGluings (read n) tets

getRequiredAttrValue :: ArrowXml a => String -> a XmlTree String
getRequiredAttrValue a =
    getAttrValue0 a 
    `HXT.orElse`
    constA (throw (RequiredAttributeNotPresentException a))

translateGluings :: Word -> [[Int]] -> Triangulation
translateGluings ntet gluingRows = 
    assert (ntet == fi (length gluingRows)) $ res
    where
        res =
                mkTriangulation 
                    ntet
                    (concatMap translateGluingForTet (zip tindices gluingRows))


        tetIxInBounds tet = tet >= 0 && tet < tindex ntet

        tindices | ntet==0 = []
                 | otherwise = [0..tindex (ntet-1)]


        translateGluingForTet :: (TIndex, [Int]) -> [Gluing] 
        translateGluingForTet (tet, gluingRow) =
            assert (length (gluingRow) == 8) $
            assert (tetIxInBounds tet) $
            mapMaybe translateGluing (zip [0..3] (chunk 2 gluingRow))

            where

                translateGluing :: (Int,[Int]) -> Maybe Gluing
                translateGluing (_, [-1,-1]) = Nothing
                translateGluing (faceIx, [fromIntegral -> tet',dcba]) = 
                    assert (faceIx >= 0 && faceIx < 4) $
                    assert (tetIxInBounds tet') $
                    let
                        s4 = s4fromInt dcba

                        vis :: Triple Index4
                        vis = deleteAt4 faceIx allIndex4'

                        vs,us :: Triple Vertex
                        vs = map3 index4ToVertex vis

                        us = map3 (index4ToVertex . (tupleToFun4 s4)) vis

                        res_ :: Gluing
                        res_ =
                                (tet ./ triangle vs, 
                                 tet' ./ otriangle us)

                    in
                       -- Debug.Trace.trace (show res) 
                        (Just res_)

                translateGluing _ = assert False undefined
            

data RequiredAttributeNotPresentException = RequiredAttributeNotPresentException String
    deriving (Show,Typeable)

instance Exception RequiredAttributeNotPresentException








-- g ::  (Ord t2, Read t2) => Element -> (t2, t2, t2, t2)
-- g = fromList4 . sort . fmap read . words . strContent

    

-- test :: IO [Triangulation]
-- test = readRgaFile "/h/dev/regina-things/sfsHakenExample2Triang.rga"


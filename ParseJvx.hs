{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, TemplateHaskell #-}    
module ParseJvx where

import Control.Monad
import Control.Monad.Compose.Class
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Error
import Data.Functor
import Data.List(sort)
import Data.Map as Map
import Data.Maybe
import HomogenousTuples
import Text.XML.Light hiding(parseXMLDoc,strContent)
import Text.XML.Monad
import Triangulation
import TupleTH
import qualified Data.ByteString as BS

readJvxFile fn = do
    s <- BS.readFile fn 
    return (runXml (parseXMLDoc >>> parseJvx) s)

findsFs = findElementNameUI "jvx-model" 
            >>> findElementNameUI "geometries"
            >>> findElementNameUI "geometry"
            >>> findElementNameUI "faceSet"
            >>> findElementNameUI "faces"
            >>> findElementsNameUI "f"

parseJvx = (convertToTriangulation . fmap g) `liftM` findsFs
    where
        g = fromList4 . sort . fmap read . words . strContent
    
convertToTriangulation :: [Quadruple Int] -> Triangulation
convertToTriangulation tets = fromRight $ mkTriangulationG tets gluings
    where
        triangleToProj :: Triangle -> Quadruple Int -> Triple Int
        triangleToProj (vertices -> vs) tet = map3 (\v -> vertexToProj v tet) vs 

        vertexToProj v | v == vA = $(proj 4 0)
                       | v == vB = $(proj 4 1)
                       | v == vC = $(proj 4 2)
                       | v == vD = $(proj 4 3)

        glueHelperMap :: Map (Triple Int) [(Quadruple Int, Triangle)]
        glueHelperMap = Map.fromListWith (++) 
                            [ (x,[y])
                                | tet <- tets
                                , t <- allTriangles
                                , let x = triangleToProj t tet
                                , let y = (tet,t)
                            ]

        gluings = catMaybes . fmap f . Map.toList $ glueHelperMap 

        f (_,[_]) = Nothing
        f (_,[(tet1,t1),(tet2,t2)]) = Just (tet1,t1,tet2,S3abc,t2)
        f (triple,tets) = error ("Triangle "++show tets++" is contained in more than two tetrahedra: "++show tets)



test = asXmlError <$> readJvxFile "/h/diplomarbeit/PoincareSphere.16.facets.jvx"

asXmlError :: Either XmlError a -> Either XmlError a
asXmlError = id

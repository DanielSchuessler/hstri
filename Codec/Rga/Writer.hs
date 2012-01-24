{-# LANGUAGE FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Codec.Rga.Writer(
    ToRga(..),
    toRgaString,
    writeRgaFile,
    
    
    
    -- | Reexports
    xrender,Xml,
    )
    
    where

import Triangulation
import Codec.Rga.Util
import Text.XML.Generator
import HomogenousTuples
import Data.Tuple.Index
import Element
import Data.List
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8(toString)


gluingsToRga :: Triangulation -> TIndex -> RgaGluingsForTet 
gluingsToRga tr tet_dom =
        map4 f allIndex4'
  where
    f i_dom = 
        fmap g (lookupGluingOfITriangle tr glDom_)

      where
        v_dom = index4ToVertex i_dom 

        glDom_ = tet_dom ./ triangleByDualVertex v_dom

        g glCod_ = (tet_cod, tupleFromFun4 h)
          where
            tet_cod = getTIndex $ glCod_
            v_cod = oTriangleDualVertex . unI $ glCod_

            h (index4ToVertex -> v) = vertexToIndex4
                    (if v == v_dom
                        then v_cod
                        else gluingMap (glDom_,glCod_) v)


rgaGluingsForTetToString :: RgaGluingsForTet -> String
rgaGluingsForTetToString =
    intercalate " " . map (maybe "-1 -1"
                            (\(tet,s4) -> show tet ++ " " ++ show (s4toInt s4)))

                    . asList
                    


instance ToRga LabelledTriangulation where 
    toRga (l,tr) =
        xelem "packet"
           (xattrs [ xattr "label" l
                   , xattr "type" "Triangulation"
                   , xattr "typeid" "3" ]

            <#> xtetrahedra tr)
                        
xtetrahedra :: Triangulation -> Xml Elem
xtetrahedra tr = 
    xelem "tetrahedra" (
                    xattr "ntet" (show . tNumberOfTetrahedra_ $ tr) 
                <#> xelems
                    [   xelem "tet" (
                            xattr "desc" ""
                        <#> xtext txt)

                     |  tet <- tTetrahedra_ tr,
                        let txt = rgaGluingsForTetToString (gluingsToRga tr tet)


                    ])


instance ToRga Triangulation where
    toRga tr = toRga ("Anonymous triangulation",tr)

xrender' :: Renderable r => Xml r -> String
xrender' = toString . xrender 


class ToRga a where
    toRga :: a -> Xml Elem

toRgaString :: ToRga a => a -> String
toRgaString = xrender' . toRga

writeRgaFile :: ToRga a => FilePath -> a -> IO ()
writeRgaFile fn = BL.writeFile fn . xrender . wrapUp . toRga 

wrapUp :: AddChildren (Xml Attr, b) => b -> Xml Doc
wrapUp x = 
    doc defaultDocInfo
        (xelem "reginadata"
            (xattr "engine" "4.9.0"
             <#> x))

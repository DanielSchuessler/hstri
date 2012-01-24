{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, NoMonomorphismRestriction #-}
module Triangulation.Transformations 

    where

import Triangulation
import THUtil
import HomogenousTuples
import Util
import FileLocation

deleteTetSafe (k :: TIndex) tr =
    $(assrt [| k < tNumberOfTetrahedra tr |] [[|k|],[|tNumberOfTetrahedra tr :: Int|]]) $ 
    let    n = tNumberOfTetrahedra_ tr
    in let reindex 
            | fi k == n-1 = id
            | otherwise = 
                map (mapTIndices 
                        (\i -> if i < k
                                  then i
                                  else i-1))

    in let gluings' = 
            reindex 
            . filter (\gl -> not $ k `elem2` map2 ($ gl) (glDomTet,glCodTet))
            . tOriginalGluings $ tr

                
    in
        mkTriangulationSafe (n-1) gluings'  


deleteTet = (.) $(fromRht) . deleteTetSafe




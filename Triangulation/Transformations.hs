{-# LANGUAGE ViewPatterns, ScopedTypeVariables, TemplateHaskell, NoMonomorphismRestriction #-}
module Triangulation.Transformations 

    where

import Triangulation.AbstractNeighborhood
import Control.Exception
import Control.Failure
import Data.Function
import EitherC
import FileLocation
import HomogenousTuples
import MathUtil
import THUtil
import Triangulation
import Triangulation.PreTriangulation
import TriangulationCxtObject
import Util
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty(NonEmpty(..))
import Control.Arrow


 
deleteTet'
  :: TIndex
     -> Triangulation
     -> EitherC (Located TriangulationConstructionError) Triangulation
deleteTet' (k :: TIndex) tr =
    $(assrt [| k < tNumberOfTetrahedra tr |] [[|k|],[|tNumberOfTetrahedra tr :: Int|]]) $ 
    let    n = tNumberOfTetrahedra_ tr

    in let gluings' = 

              map (mapTIndices (codegeneracy k)) 
            . filter (\gl -> not $ k `elem2` map2 ($ gl) (glDomTet,glCodTet))
            . tGluingsIrredundant $ tr

                
    in
        mkTriangulationSafe (n-1) gluings'  


deleteTet :: TIndex -> Triangulation -> Triangulation
deleteTet = (.) $(unEitherC) . deleteTet'


-- pachner32 tr e =
--         case innerEdgeNeighborhood' tr (toOrderedFace e) of
--              Nothing -> $(failureStr) ("Pachner 3-2 move not allowed on boundary edge "++show e)
--              Just [ient0,ient1,ient2] -> go ient0 ient1 ient2
--              Just tets -> $(failureStr) ("Pachner 3-2 move: Edge "++show e++" doesn't have degree 3, but "
--                                     ++ show (length tets))
-- 
--     where
-- 
--         go 
--             ient0@(viewI -> I i0 ent0)
--             ient1@(viewI -> I i1 ent1)
--             ient2@(viewI -> I i2 ent2)
-- 
--             = 
--               let
--                 (===) = (==) `on` pMap tr
--               in
-- 
--                 assert (and [ f ient0 === f ent | f <- [ient_bot,ient_top], ent <- [ient0,ient1] ]) $
--                 assert (and [ ient_right ient === ient_left ient' | 
--                                 
--                                 let ients = [ient0,ient1,ient2],
--                                 (ient,ient') <- zip ients (tail (cycle ients)) ]) $
-- 
-- 
--                 let
-- 
                    

                
        

         
-- | Also returns the new edge glued to the given edge
pt_layerOn
  :: OIEdge -> Triangulation -> EitherC (Located ErrorCall) (Triangulation,OIEdge)
pt_layerOn e tr = 
    case edgeNeighborhood tr e of
         Right _ -> $(err') "tr_layerOn requires a boundary edge"
         Left (ben_toNonEmpty -> 
               (NE.head &&& NE.last) -> 
               (ient_leftTri *** ient_rightTri) ->
               (ltri,rtri))

                ->

            do
                (tr',i) <- addTet (\i -> 
                                [ oiTriangleGluing ltri (i ./ oABC)
                                , oiTriangleGluing rtri (i ./ oABD)
                                ]) tr

                return (tr',i./oBA)




                




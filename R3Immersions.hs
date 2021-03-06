{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, ViewPatterns, RecordWildCards, NamedFieldPuns, ScopedTypeVariables, TypeSynonymInstances, NoMonomorphismRestriction, TupleSections, StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveDataTypeable, MultiParamTypeClasses #-}
module R3Immersions where

import MathUtil
import Numeric.AD.Vector
import Data.VectorSpace
import Tetrahedron.Vertex
import Tetrahedron.Edge
import ConcreteNormal
import Util
import R3Immersions.Simplices

semidisk
  :: (RealFloat (Scalar v), Show (Scalar v), VectorSpace v) =>
     v -> v -> v -> Tup2 (Scalar v) -> v
semidisk im00 im01 im10 ab =
    let
        Tup2 (x,y) = standardSemidisk ab

        -- (x,y) = c00 (1,0) + c01 * (0,-1) + c10 * (0,1)
        -- c00 + c01 + c10 = 1

        -- <=>
        -- x = c00
        -- y = c10 - c01 
        -- c00 + c01 + c10 = 1

        c00 = x
        c01 = (1 - x - y)/2
        c10 = y + c01
    in
        c00 *^ im00
        ^+^
        c01 *^ im01
        ^+^
        c10 *^ im10
        

    
        

-- | Maps:
--
-- * (0,0) to (1,0)
--
-- * (0,1) to (0,-1)
--
-- * (1,0) to (0,1)
--
-- Input is assumed to be in the unit 2-simplex
standardSemidisk (Tup2 (aness,bness)) =
            let


                -- runs from 0 to 1
                abness = aness+bness 

                abnessDistorted = abness


                -- runs from -1 to 1
                a_vs_b = if abness == 0
                            then 0
                            else (aness-bness)/abness 


                xz_0 = -- if we're close to c (low abness), draw circular arcs
                                (let 
                                    phimax = acos (abnessDistorted/2) 
                                    foo = cos (a_vs_b*phimax) * abnessDistorted
                                    bar = sin (a_vs_b*phimax) * abnessDistorted
                                 in  tup2 (1-foo) bar)

                xz_1 = -- if we're close to ab, draw lines
                                (let x = 1-abnessDistorted 
                                     zbounds = sqrt' (1 - x^2)
                                     -- x^2 + zbounds^2 = 1
                                     z = zbounds*a_vs_b 
                                 in
                                    tup2 x z)

                xz@(Tup2 (x, z)) = slerpG abness xz_0 xz_1
                                
            in
                xz



-- | x0 and x1 map to the equator, x2 to the top, x3 to the bottom 
standardSnapped3Ball (Tup4 (x0, x1, x2, x3)) = 
    v_xy  ^+^ (x2-x3) *^ tup3Z

    where
        a = x0 + x1 
        phi_min = 0
        phi_max = 2*pi
        phi = phi_min + (phi_max-phi_min) * x0/a
        v_xy | a > 0 = a *^ tup3 (- (sin phi)) (cos phi) 0
             | otherwise = zeroV


torusCoords' :: Floating a => a -> a -> SolidTorusPoint a -> Tup3 a
torusCoords' major minor (STP long lat boundaryness) =
    let
        minor' = minor * boundaryness 
        r = major + cos lat * minor' 
    in
        tup3 (cos long * r) (sin long * r) (sin lat * minor') 




embedNCorner :: Fractional a1 => NmCorner -> Tup4 a1
embedNCorner c = 
    embedEd 
        (edge . unI . c_type $ c)
        (interpol t tup2X tup2Y)

    where
        t =
         ((fi . unPos . c_pos) c + 1)
            /
         ((fi . cornersOfSameType) c + 1)




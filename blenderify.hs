{-# LANGUAGE NoMonomorphismRestriction, GADTs, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving #-} 
import DeltaSet
import SimplicialComplex
import Data.Vect.Double
import Blender
import Data.NaturalNumber
import TupleTH
import AbstractTetrahedron
import HomogenousTuples
import Data.Function
import Control.Applicative
import Data.List

data V = P0' | P0 | PL' | PL | PR' | PR | P1' | P1
    deriving (Eq,Ord,Show,Enum)

aff :: Vector g => g -> g -> Double -> g
aff a b p = p *& a &+ (1-p) *& b 

list4 = (,,,) 

prism a' a b' b c' c = [ list4 a' a b c, list4 a' b' b c, list4 a' b' c' c ] 

x = 
    Pseudomanifold $
    addCoordFunc coordFunc $


    fromTets (prism P0' P0 PL' PL P1' P1 ++
              prism P0' P0 PR' PR P1' P1)

w = 6
h = 4
d = 5

coordFunc v = case v of
                    P0 -> Vec3 0 (-h) 0
                    PL -> Vec3 (-w) 0 0
                    PR -> Vec3 w 0 0
                    P1 -> Vec3 0 h 0
                    _ -> coordFunc (succ v) &+ Vec3 0 0 (2*d)

-- instance Show v => BlenderLabel (ByTetrahedra v) where
--         blenderLabel _ NZero = show
--         blenderLabel _ (NSuccessorTo _) = show . lToList 

--main = testBlender (defaultScene x)
main = testBlender . 
       defaultScene .
       transformCoords ((&+ Vec3 (-0.66) (-2.7) (-0.52)) . rot) $
       (Pseudomanifold abstractTet
        `DisjointUnion`
        abnormalCurve)

rot = withOrigin (Vec3 0.5 0.5 0.5) (rotate3 (pi*0.42) vec3Z)

deriving instance Ord Vec3

sort2 = $(tupleFromList 2) . sort . $(tupleToList 2)
sort3 :: (Vec3, Vec3, Vec3) -> (Vec3, Vec3, Vec3)
sort3 = $(tupleFromList 3) . sort . $(tupleToList 3)

normalTris = NormalSurface $
    let
        f v = (inter 0.4 v v0, inter 0.4 v v1, inter 0.4 v v2)
            where
                (v0,v1,v2) = vertices (triangle v)
    in
        fromTris (sort3 . f <$> allVertices)

co = coords abstractTet
inter p = interpolate p `on` co

abnormalCurve = NormalSurface $ fromEdges (sort2 <$> [(u,v),(v,w)])
    where
        u = inter 0.25 a b
        v = inter 0.4 a c
        w = inter 0.55 a b

        a = vC
        b = vA
        c = vB

withOrigin o f = (&+ o) . f . (&- o)




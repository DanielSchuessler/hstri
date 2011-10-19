{-# LANGUAGE NoMonomorphismRestriction, GADTs, ScopedTypeVariables #-} 
import Simplicial
import List
import Data.Vect.Double
import Blender
import Data.NaturalNumber

data V = P0' | P0 | PL' | PL | PR' | PR | P1' | P1
    deriving (Eq,Ord,Show,Enum)

aff :: Vector g => g -> g -> Double -> g
aff a b p = p *& a &+ (1-p) *& b 

prism a' a b' b c' c = [ list4 a' a b c, list4 a' b' b c, list4 a' b' c' c ] 

x = ByTetrahedra (prism P0' P0 PL' PL P1' P1 ++
                  prism P0' P0 PR' PR P1' P1)

w = 6
h = 4
d = 5

instance VertexInfoClass V where
    vertexInfo v = VertexInfo TriangulationSimplex (coords0 v)
        where 
            coords0 v = case v of
                    P0 -> Vec3 0 (-h) 0
                    PL -> Vec3 (-w) 0 0
                    PR -> Vec3 w 0 0
                    P1 -> Vec3 0 h 0
                    _ -> coords0 (succ v) &+ Vec3 0 0 (2*d)

instance Show v => BlenderLabel (ByTetrahedra v) where
        blenderLabel _ NZero = show
        blenderLabel _ (NSuccessorTo _) = show . lToList 

main = testBlender (Scene x)

fm0 = faceMap x NZero
fm1 = faceMap x (NSuccessorTo NZero)


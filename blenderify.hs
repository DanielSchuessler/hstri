{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, GADTs, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving #-} 
import AbstractTetrahedron
import Blender
import Blenderable
import Control.Applicative
import Data.Function
import Data.List
import Data.NaturalNumber
import Data.Vect.Double
import DeltaSet
import DisjointUnion
import HomogenousTuples
import Layout
import SimplexLabels
import SimplicialComplex
import System.Exit
import TupleTH
import TypeLevel.TF
import GraphComplex
import ExampleComplexes

data V = P0' | P0 | PL' | PL | PR' | PR | P1' | P1
    deriving (Eq,Ord,Show,Enum)

aff :: Vector g => g -> g -> Double -> g
aff a b p = p *& a &+ (1-p) *& b 

list4 = (,,,) 

prism a' a b' b c' c = [ list4 a' a b c, list4 a' b' b c, list4 a' b' c' c ] 

x = 
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

deriving instance Ord Vec3

sort2 = $(tupleFromList 2) . sort . $(tupleToList 2)
sort3 :: (Vec3, Vec3, Vec3) -> (Vec3, Vec3, Vec3)
sort3 = $(tupleFromList 3) . sort . $(tupleToList 3)

normalTris :: Blenderable (OTuple Vec3)
normalTris = normalSurface' $
    let
        f v = (inter 0.4 v v0, inter 0.4 v v1, inter 0.4 v v2)
            where
                (v0,v1,v2) = vertices (triangle v)
    in
        fromTris (sort3 . f <$> allVertices)


inter :: Double -> Vertex -> Vertex -> Vec3
inter p = interpolate p `on` co
    where
        co :: Vertex -> Vec3
        co = vertlbl tet3d

abnormalCurve = normalSurface' $ fromEdges (sort2 <$> [(u,v),(v,w)])
    where
        u = inter 0.25 a b
        v = inter 0.4 a c
        w = inter 0.55 a b

        a = vC
        b = vA
        c = vB

withOrigin :: AbelianGroup b => b -> (b -> b) -> b -> b
withOrigin o f = (&+ o) . f . (&- o)



main = testBlender . 
       defaultScene .
       transformCoords ((&+ Vec3 (-0.66) (-2.7) (-0.52)) . rot) $
       (pseudomanifold (baryFlat tet3d)
        --`disjointUnion` abnormalCurve
        )
  where
    rot = withOrigin (Vec3 0.5 0.5 0.5) (rotate3 (pi*0.42) vec3Z)


--testLayout :: (Show (Vert a), ShowN a) => DeltaSet a -> IO ExitCode
testLayout ds = do
    wc <- layoutDebug ds
    testBlender . defaultScene . pseudomanifold $ wc

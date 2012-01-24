{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, TupleSections, FlexibleContexts, TypeFamilies, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module Simplicial.DeltaSet2(
    module Simplicial.DeltaSet1,
    DeltaSet2(..),
    face21,
    AnySimplex2,
    AnySimplex2Of,
    faces20,
    faces20Ascending,
    foldAnySimplex2,
    vertToAnySimplex2,
    edToAnySimplex2,
    triToAnySimplex2,
    foldAnySimplex2',
    biMapAnySimplex2,
    anySimplex1To2,
    anySimplex2s,
    TrianglesContainingEdge_Cache,
    lookupTrianglesContainingEdge,
    mkTCEC,
    Unit2Simplex(..),
    Unit2SimplexPoint,

    -- * Testing
    polyprop_faces21
    ) where

import Simplicial.DeltaSet1
import QuickCheckUtil
import Test.QuickCheck
import Control.Arrow
import qualified Data.Map as M
import Control.Applicative
import PrettyUtil
import Language.Haskell.TH.Lift
import ShortShow
import MathUtil
import Data.Tuple.OneTuple
import Data.Vect.Double.Base

class (DeltaSet1 s, Triangles s) => 
    DeltaSet2 s where

    faces21 :: s -> Tri s -> Triple (Ed s)

face21 :: DeltaSet2 a => a -> Tri a -> Index3 -> Ed a
face21 = (.) tupleToFun3 . faces21

newtype AnySimplex2 v e t = AnySimplex2 (Either (AnySimplex1 v e) t) 
    deriving(Show,SubSumTy,SuperSumTy,Eq,Ord)

type instance L (AnySimplex2 v e t) = AnySimplex1 v e 
type instance R (AnySimplex2 v e t) = t


instance (Pretty v, Pretty e, Pretty t) => Pretty (AnySimplex2 v e t) where
    prettyPrec prec = foldAnySimplex2' (prettyPrec prec) (prettyPrec prec)

type AnySimplex2Of s = AnySimplex2 (Vert s) (Ed s) (Tri s)


foldAnySimplex2'
  :: (AnySimplex1 v e -> r) -> (t -> r) -> AnySimplex2 v e t -> r
foldAnySimplex2' = (||||)

foldAnySimplex2 :: 
    (v -> r) -> (e -> r) -> (t -> r) -> AnySimplex2 v e t -> r
foldAnySimplex2 kv ke kt (AnySimplex2 x) = (foldAnySimplex1 kv ke ||| kt) x 

anySimplex1To2 :: AnySimplex1 v e -> AnySimplex2 v e t
anySimplex1To2 = left'

vertToAnySimplex2 :: v -> AnySimplex2 v e t
vertToAnySimplex2 = anySimplex1To2 . vertToAnySimplex1

edToAnySimplex2 :: e -> AnySimplex2 v e t
edToAnySimplex2 = anySimplex1To2 . edToAnySimplex1

triToAnySimplex2 :: t -> AnySimplex2 v e t
triToAnySimplex2 = right'

polyprop_faces21
  :: (Eq (Element (Verts s)),
      Show tris,
      Show (Element (Verts s)),
      DeltaSet2 s,
      Tris s ~ [tris]) =>
     s -> Property
polyprop_faces21 s = 
    forAllElements (triangles s)
        (\t -> case faces21 s t of
                    es ->
                        case map3 (faces10 s) es of
                             ( (v0_0,v1_0)
                              ,(v0_1,v1_1)
                              ,(v0_2,v1_2)
                              ) ->

                                 v1_1 .=. v1_2 -- 0
                                 .&.
                                 v1_0 .=. v0_2 -- 1
                                 .&.
                                 v0_0 .=. v0_1 -- 2
                                 )
                                 
                                    



faces20
  :: DeltaSet2 s =>
     s -> Tri s -> (Vert s, Vert s, Vert s)
faces20 t tr = (v2,v1,v0)
    where
        (e12,e02,_) = faces21 t tr
        (v2,v1) = faces10 t e12
        (_,v0) = faces10 t e02

faces20Ascending
  :: DeltaSet2 s =>
     s -> Tri s -> (Vert s, Vert s, Vert s)
faces20Ascending t = reverse3 . faces20 t 



newtype TrianglesContainingEdge_Cache ed tri = 
    TCEC { lookupTrianglesContainingEdge :: ed -> [(tri,Index3)] } 



mkTCEC
  :: (Ord (Ed s), DeltaSet2 s) => s -> TrianglesContainingEdge_Cache (Ed s) (Tri s)
mkTCEC s = TCEC (m M.!)
    where
        m = M.fromListWith (++)
                    . concatMap (\t -> 
                        asList 
                            (zipTuple3 
                                (faces21 s t) 
                                (map3 ((:[]) . (t,)) allIndex3')))
                    $ triangleList s
                

anySimplex2s
  :: (Vertices s, Triangles s, Edges s) =>
     s -> [AnySimplex2 (Vert s) (Ed s) (Tri s)]
anySimplex2s = 
    (++)
    <$> (map anySimplex1To2 . anySimplex1s) 
    <*> (map triToAnySimplex2 . triangleList)



biMapAnySimplex2
  :: (AnySimplex1 v e -> AnySimplex1 v' e')
     -> (t -> t') -> AnySimplex2 v e t -> AnySimplex2 v' e' t'
biMapAnySimplex2 = (++++)


instance (Lift v, Lift e, Lift t) => Lift (AnySimplex2 v e t) where

    lift = either' 
            (either' 
                (\x -> [| vertToAnySimplex2 x |])
                (\x -> [| edToAnySimplex2 x |]))
            (\x -> [| triToAnySimplex2 x |])

instance (ShortShow v, ShortShow e, ShortShow t) => ShortShow (AnySimplex2 v e t) where
    shortShow = foldAnySimplex2' shortShow shortShow 

data Unit2Simplex = Unit2Simplex
    deriving Show

instance Vertices Unit2Simplex where
    type Verts Unit2Simplex = Triple Unit2SimplexPoint
    vertices Unit2Simplex = faces20 Unit2Simplex Unit2Simplex

instance Edges Unit2Simplex where
    type Eds Unit2Simplex = Triple (Unit2SimplexPoint,Unit2SimplexPoint)
    edges Unit2Simplex = faces21 Unit2Simplex Unit2Simplex

instance Triangles Unit2Simplex where
    type Tris Unit2Simplex = OneTuple Unit2Simplex
    triangles Unit2Simplex = OneTuple Unit2Simplex

instance DeltaSet1 Unit2Simplex where
    faces10 _ = reverse2

instance DeltaSet2 Unit2Simplex where
    faces21 _ _ = ((b,c),(a,c),(a,b))
        where
            a = Vec2 0 0
            b = Vec2 0 1
            c = Vec2 1 0


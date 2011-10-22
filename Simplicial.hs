{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies, GADTs #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, NoMonomorphismRestriction, ScopedTypeVariables #-} 
module Simplicial where
import Test.QuickCheck
import Data.Set as Set
import Data.Sequence as Seq
import Text.XML.Generator
import Data.Vect.Double
import Data.Map as Map
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as B8
import Control.Exception
import Data.Typeable
import Data.Foldable as Fold
import List
import Text.Printf
import Control.Arrow
import Nat


type family Simplex a n

class DeltaSet a where
    faceMap :: a -> N n -> Int -> Simplex a (SuccessorTo n) -> Simplex a n 
    simplices :: a -> N n -> [Simplex a n] 

type Vertex a = Simplex a Zero

vertices :: DeltaSet a => a -> [Vertex a]
vertices = flip simplices n0 

type Edge a = Simplex a One

edges :: DeltaSet a => a -> [Edge a]
edges = flip simplices n1
                                  
type Triangle a = Simplex a Two

triangles :: DeltaSet a => a -> [Triangle a]
triangles = flip simplices n2

class DeltaSet a => SimplicialSet a where
    degeneracyMap :: a -> N n -> Int -> Simplex a n -> Simplex a (SuccessorTo n)  




data DisjointUnion a1 a2 = DisjointUnion a1 a2


type instance Simplex (DisjointUnion a1 a2) n = Either (Simplex a1 n) (Simplex a2 n)

instance (DeltaSet a1, DeltaSet a2) => DeltaSet (DisjointUnion a1 a2) where
    faceMap (DisjointUnion a1 a2) n i = (+++) (faceMap a1 n i) (faceMap a2 n i)
    simplices (DisjointUnion a1 a2) n = 
         (fmap Left (simplices a1 n)) ++ (fmap Right (simplices a2 n))

instance (SimplicialSet a1, SimplicialSet a2) => SimplicialSet (DisjointUnion a1 a2) where
    degeneracyMap (DisjointUnion a1 a2) n i = (+++) (degeneracyMap a1 n i) (degeneracyMap a2 n i)
-- 
-- newtype WrappedSeq v n = WrappedSeq (Seq v) 

-- newtype AdjoinDegeneracies a = AdjoinDegeneracies a
-- 
-- data FormalDegeneracy a = FormalDegeneracy Int a
-- 
-- instance (DeltaSet a) => DeltaSet (AdjoinDegeneracies a) where
--     type Simplex (AdjoinDegeneracies a) = Either (Simplex a) (FormalDegeneracy (Simplex a))


data EmptySimplicialSet = EmptySimplicialSet

-- data EmptySimplicialSetSimplex n
-- 
-- deriving instance Eq (EmptySimplicialSetSimplex n)
-- deriving instance Ord (EmptySimplicialSetSimplex n)


type instance Simplex EmptySimplicialSet n = Void

instance DeltaSet EmptySimplicialSet where
    faceMap _ _ _ = undefined
    simplices _ _ = []


data ByTetrahedra v = ByTetrahedra [List v Four]

-- data Simplex_ByTetrahedra v n where
--     Simplex_ByTetrahedra0 :: v -> Simplex_ByTetrahedra0 v Zero 
--     Simplex_ByTetrahedra4 :: (v -> Simplex_ByTetrahedra0 v Zero 

data Void

deriving instance Eq Void
deriving instance Ord Void

btEmpty :: ByTetrahedra v
btEmpty = ByTetrahedra []



-- newtype VertexDefinedSimplex v n = VertexDefinedSimplex (List v (SuccessorTo n))
--     deriving(Show,Eq,Ord)
-- 
-- 
-- unVertexDefinedSimplex (VertexDefinedSimplex l) = l 

nub' = Set.toList . Set.fromList


type instance Simplex (ByTetrahedra v) Zero = v
type instance Simplex (ByTetrahedra v) (SuccessorTo n) = List v (SuccessorTo (SuccessorTo n))


btFaceMap :: N n -> Int -> Simplex (ByTetrahedra v) (SuccessorTo n) -> Simplex (ByTetrahedra v) n 
btFaceMap n = case n of                                               
                   NZero ->                                          
                       \i (v0 ::: v1 ::: Nil) -> case i of           
                                  0 -> v1                            
                                  1 -> v0                            
                                                                     
                   NSuccessorTo _ -> case mkNaturalNumberDict n of   
                                        NaturalNumberDict -> ldelete 

instance Ord v => DeltaSet (ByTetrahedra v) where

    faceMap _ = btFaceMap 
                  
                  
                  
                  

                  
                  

    simplices (ByTetrahedra tets) n =
        case n of
            NZero ->
                nub' .
                Prelude.concatMap lToList $ tets

            NSuccessorTo NZero ->
                collect (\(a ::: b ::: c ::: d ::: Nil) ->
                    [ list2 a b, list2 a c, list2 a d, list2 b c, list2 b d, list2 c d ])
                                
            NSuccessorTo (NSuccessorTo NZero) ->
                collect (\t ->
                    fmap (flip ldelete t) [0..3])

            NSuccessorTo (NSuccessorTo (NSuccessorTo NZero)) -> tets

            _ -> []
                     

      where
        collect :: (List v Four -> [List v n]) -> [List v n]
        collect f = nub' . Prelude.concatMap f $ tets


data NormalDisc t ann = Tri (Simplex t N4) Int ann | Quad (Edge t) (Edge t) ann

data NormalSurf t ann = NormalSurf t [NormalDisc t ann]

type instance Simplex (NormalSurf t ann) Zero = Edge t
type instance Simplex (NormalSurf t ann) N1 = (Edge t, Edge t)
type instance Simplex (NormalSurf t ann) N2 = NormalDisc t ann

faceMap0 t = faceMap t NZero
faceMap1 t = faceMap t typeLevel1
faceMap2 t = faceMap t typeLevel2
faceMap3 t = faceMap t typeLevel3

faces :: DeltaSet t => 
    t -> N n -> Simplex t (SuccessorTo n) -> List (Simplex t n) (SuccessorTo (SuccessorTo n))

faces t n x = mapList (\i -> faceMap t n i x) (range 0 (NSuccessorTo (NSuccessorTo n)))

verts2 t x = mapList f ( (3,2,1) ::: (3,2,0) ::: (3,1,0) ::: (2,1,0) ::: Nil )
    where
        f (i2,i1,i0) = faceMap0 t i0 . faceMap1 t i1 . faceMap2 t i2 $ x

-- instance (Ord (Vertex t), DeltaSet t) => DeltaSet (NormalSurf t ann) where
--     faceMap _ NZero i = case i of
--                             0 -> snd
--                             1 -> fst
-- 
--     faceMap (NormalSurf ul _) (SuccessorTo NZero) i d =  
--         case d of
--              NormalTri t j -> 
--                 case verts4 ul t of 
--                      v0 ::: v1 ::: v2 ::: v3 ::: Nil


        



typeLevel1 = NSuccessorTo NZero       
typeLevel2 = NSuccessorTo typeLevel1
typeLevel3 = NSuccessorTo typeLevel2


faceMap'
  :: forall a n. (DeltaSet a, NaturalNumber n) =>
     a -> Int -> Simplex a (SuccessorTo n) -> Simplex a n
faceMap' a = faceMap a (asN :: N n)

-- instance (NaturalNumber n, Arbitrary v) => Arbitrary (WrappedSeq v n)

-- data SimplicialComplex v =
--     SimplicialComplex {
--         vertices :: Set v
--     ,   isFace :: Set v -> Bool
--     }
-- 
-- data WrappedComplex v n where
--     Vertex :: v -> WrappedComplex v Zero
--     Simplex :: [v] -> WrappedComplex v (SuccessorTo n) 
-- 
-- instance SimplicialSet (WrappedComplex v) where
--     faceMap 0 (Simplex vs) = tail vs 

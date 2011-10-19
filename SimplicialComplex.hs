{-# LANGUAGE RecordWildCards, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables, NamedFieldPuns, TypeFamilies, DefaultSignatures, FlexibleContexts, OverlappingInstances, StandaloneDeriving, UndecidableInstances, GeneralizedNewtypeDeriving #-} 
-- | All 'Pair's, 'Triple's and 'Quadruple's are assumed to be strictly ascendingly ordered in this module.
module SimplicialComplex where
import Data.List
import Data.Vect.Double 
import Data.Vect.Double.Instances
import TupleTH
import Control.Applicative
import Data.Set as Set
import Data.Map as Map
import Control.Exception
import HomogenousTuples
import Control.Monad
import AbstractTetrahedron
import DeltaSet




class SimplicialComplex1 a where
    getSC1 :: a -> SC1 (Vert a)


data SC1 v = SC1 {
    s0_ :: [v],
    s1_ :: [Pair v],
    super01_ :: v -> [Pair v]
}


type instance Vert (SC1 v) = v

instance SimplicialComplex1 (SC1 v) where
    getSC1 = id

super01 = super01_ . getSC1




class SimplicialComplex1 a => SimplicialComplex2 a where
    getSC2 :: a -> SC2_ (Vert a)

data SC2_ v = SC2_ {
    s2_ :: [Triple v],
    super12_ :: Pair v -> [Triple v] 
}

super12 = super12_ . getSC2

data SC2 v = SC2 {
    sc2_1 :: SC1 v,
    sc2_2 :: SC2_ v
}


type instance Vert (SC2 v) = v

instance SimplicialComplex1 (SC2 v) where 
    getSC1 = sc2_1

instance SimplicialComplex2 (SC2 v) where
    getSC2 = sc2_2


class SimplicialComplex2 a => SimplicialComplex3 a where
    getSC3 :: a -> SC3_ (Vert a)

data SC3_ v = SC3_ {
    s3_ :: [Quadruple v],
    super23_ :: Triple v -> [Quadruple v] 
}

super23 = super23_ . getSC3

data SC3 v = SC3 {
    sc3_1 :: SC1 v,
    sc3_2 :: SC2_ v,
    sc3_3 :: SC3_ v
}


type instance Vert (SC3 v) = v
    
instance SimplicialComplex1 (SC3 v) where 
    getSC1 = sc3_1

instance SimplicialComplex2 (SC3 v) where
    getSC2 = sc3_2

instance SimplicialComplex3 (SC3 v) where
    getSC3 = sc3_3


data ComplexPlus a b = ComplexPlus {
    cp_complex :: a,
    cp_extra :: b
}
    deriving Show


type instance Vert (ComplexPlus a b) = Vert a

instance SimplicialComplex1 a => SimplicialComplex1 (ComplexPlus a b) where
    getSC1 = getSC1 . cp_complex

instance SimplicialComplex2 a => SimplicialComplex2 (ComplexPlus a b) where
    getSC2 = getSC2 . cp_complex

instance SimplicialComplex3 a => SimplicialComplex3 (ComplexPlus a b) where
    getSC3 = getSC3 . cp_complex

-- data SimplicialComplex2 v vlbl = SimplicialComplex2 {
--     vlbl :: v -> vlbl,
--     s0 :: [v],
--     s1 :: [(v,v)],
--     s2 :: [(v,v,v)],
--     super01 :: v -> [(v,v)],
--     super12 :: (v,v) -> [(v,v,v)]
-- }
--     deriving(Functor)
-- 
-- data SimplicialComplex3 v vlbl = SimplicialComplex3 {
--     skeleton2 :: SimplicialComplex2 v vlbl,
--     s3 :: [(v,v,v,v)],
--     super23 :: (v,v,v) -> [(v,v,v,v)]
-- }
--     deriving(Functor)
-- 
-- 
-- 
-- s1lbl :: SimplicialComplex3 t vlbl -> [(vlbl, vlbl)]
-- s1lbl c = fmap ($(mapTuple 2) (vlbl c)) (s1 c)
-- 
-- s2lbl :: SimplicialComplex3 t vlbl -> [(vlbl, vlbl, vlbl)]
-- s2lbl c = fmap ($(mapTuple 3) (vlbl c)) (s2 c)
--     

-- memoComplex :: Ord v => SimplicialComplex3 v vlbl -> SimplicialComplex3 v vlbl
-- memoComplex s@SimplicialComplex3{..} = s {
--     super01 = memoFun super01 s0,
--     super12 = memoFun super12 s1,
--     super23 = memoFun super23 s2
-- }
-- 
-- 
-- 
-- 
-- 
-- 
--



data MemoComplex1 a = MemoComplex1 {
    mc1_orig :: a,
    mc1_s0 :: [(Vert a)],
    mc1_s1 :: [Pair (Vert a)],
    mc1_super01 :: Map (Vert a) [Pair (Vert a)] 
}

deriving instance (Show a, Show (Vert a)) => Show (MemoComplex1 a)

memoComplex1 :: (Ord v, SimplicialComplex1 a, v ~ Vert a) => a -> MemoComplex1 a
memoComplex1 c = MemoComplex1{..} 
    where
        mc1_orig = c
        mc1_s0 = s0 c
        mc1_s1 = s1 c
        mc1_super01 = memoFun (super01 c) mc1_s0


type instance Vert (MemoComplex1 a) = Vert a

instance Ord (Vert a) => SimplicialComplex1 (MemoComplex1 a) where
    getSC1 MemoComplex1{..} = SC1 {
        s0_ = mc1_s0,
        s1_ = mc1_s1,
        super01_ = (mc1_super01 !)
    }

newtype MemoComplex2 a = 
    MemoComplex2 (
    ComplexPlus 
        (MemoComplex1 a) 
        ([Triple (Vert a)],Map (Pair (Vert a)) [Triple (Vert a)]))



memoComplex2 :: (Ord v, SimplicialComplex2 a, v ~ Vert a) => a -> MemoComplex2 a
memoComplex2 c = MemoComplex2 $ ComplexPlus mc1 (mc2_s2,mc2_super12)
    where
        mc1 = memoComplex1 c
        mc2_s2 = s2 c
        mc2_super12 = memoFun (super12 c) (s1 mc1)

type instance Vert (MemoComplex2 a) = Vert a

instance (Ord (Vert a), SimplicialComplex1 a) => SimplicialComplex1 (MemoComplex2 a) where
    getSC1 (MemoComplex2 c) = getSC1 c


instance (Ord (Vert a), SimplicialComplex1 a) => SimplicialComplex2 (MemoComplex2 a) where
    getSC2 (MemoComplex2 c) = SC2_ {
        s2_ = fst (cp_extra c),
        super12_ = (snd (cp_extra c) !)
    }



newtype MemoComplex3 a = 
    MemoComplex3 (
    ComplexPlus 
        (MemoComplex2 a) 
        ([Quadruple (Vert a)],Map (Triple (Vert a)) [Quadruple (Vert a)]))



memoComplex3 :: (Ord v, SimplicialComplex3 a, v ~ Vert a) => a -> MemoComplex3 a
memoComplex3 c = MemoComplex3 $ ComplexPlus mc2 (mc3_s3,mc3_super23)
    where
        mc2 = memoComplex2 c
        mc3_s3 = s3 c
        mc3_super23 = memoFun (super23 c) (s2 mc2)

type instance Vert (MemoComplex3 a) = Vert a

instance (Ord (Vert a), SimplicialComplex1 a) => SimplicialComplex1 (MemoComplex3 a) where
    getSC1 (MemoComplex3 c) = getSC1 c

instance (Ord (Vert a), SimplicialComplex2 a) => SimplicialComplex2 (MemoComplex3 a) where
    getSC2 (MemoComplex3 c) = getSC2 c

instance (Ord (Vert a), SimplicialComplex2 a) => SimplicialComplex3 (MemoComplex3 a) where
    getSC3 (MemoComplex3 c) = SC3_ {
        s3_ = fst (cp_extra c),
        super23_ = (snd (cp_extra c) !)
    }


fromTets :: Ord v => [Quadruple v] -> MemoComplex3 (SC3 v)
fromTets ft_tets = assert (all isOrdered4 ft_tets) $ memoComplex3 (SC3 SC1{..} SC2_{..} SC3_{..})
    where
        s0_ = nub' . concatMap toList4 $ ft_tets
        s1_ = nub' . concatMap (toList6 . $(subtuples 4 2)) $ ft_tets
        s2_ = nub' . concatMap (toList4 . $(subtuples 4 3)) $ ft_tets 
        super01_ v = Prelude.filter ($(elemTuple 2) v) s1_
        super12_ e = Prelude.filter (\t -> $(elemTuple 3) e ($(subtuples 3 2) t)) s2_
        super23_ t = Prelude.filter (\tt -> $(elemTuple 4) t ($(subtuples 4 3) tt)) ft_tets
        s3_ = ft_tets

fromTris :: Ord v => [Triple v] -> MemoComplex2 (SC2 v)
fromTris ft_tris = assert (all isOrdered3 ft_tris) $ memoComplex2 (SC2 SC1{..} SC2_{..})
    where
        s0_ = nub' . concatMap toList3 $ ft_tris
        s1_ = nub' . concatMap (toList3 . $(subtuples 3 2)) $ ft_tris
        s2_ = ft_tris
        super01_ v = Prelude.filter ($(elemTuple 2) v) s1_
        super12_ e = Prelude.filter (\t -> $(elemTuple 3) e ($(subtuples 3 2) t)) s2_

isOrdered3 (v0,v1,v2) = v0 < v1 && v1 < v2 
isOrdered4 (v0,v1,v2,v3) = isOrdered3 (v0,v1,v2) && v2 < v3

toList6 = $(tupleToList 6)



data AbstractTet = AbstractTet
    deriving (Show)


type instance Vert AbstractTet = Vertex

instance SimplicialComplex1 AbstractTet where
    getSC1 _ = SC1 allVertices (fmap vertices allEdges) (toList3 . map3 vertices . edges)

instance SimplicialComplex2 AbstractTet where
    getSC2 _ = SC2_ (fmap vertices allTriangles) (toList2 . map2 vertices . triangles . edge)

instance SimplicialComplex3 AbstractTet where
    getSC3 _ = SC3_ [allVertices'] (const [allVertices'])

data BaryVertex1 v = Bary0 v
                   | Bary1 (Pair v)
                    deriving(Eq,Ord,Show,Functor)

data BaryVertex2 v = BaryVertex1 (BaryVertex1 v)
                   | Bary2 (Triple v)
                    deriving(Eq,Ord,Show,Functor)

data BaryVertex3 v = BaryVertex2 (BaryVertex2 v)
                   | Bary3 (Quadruple v)

                    deriving(Eq,Ord,Show,Functor)

bary3 :: (Ord v, SimplicialComplex3 a, v ~ Vert a) => 
    a -> MemoComplex3 (ComplexPlus (SC3 (BaryVertex3 v)) a)

bary3 c0 = distComplexPlus3 c c0 
    where
        c =
            fromTets [ $(catTuples 3 1)  
                        ( map3 BaryVertex2 $ $(catTuples 2 1)  
                                (map2 BaryVertex1 (Bary0 v, Bary1 e)) 
                                (Bary2 t)) 
                            (Bary3 tt)
                    |
                        v <- s0 c0,
                        e <- super01 c0 v,
                        t <- super12 c0 e,
                        tt <- super23 c0 t]


bary2 :: (Ord v, SimplicialComplex2 a, v ~ Vert a) => 
    a -> MemoComplex2 (ComplexPlus (SC2 (BaryVertex2 v)) a)

bary2 c0 = distComplexPlus2 c c0 
    where
        c =
            fromTris [ 
                        ( $(catTuples 2 1)  
                                (map2 BaryVertex1 (Bary0 v, Bary1 e)) 
                                (Bary2 t)) 
                    |
                        v <- s0 c0,
                        e <- super01 c0 v,
                        t <- super12 c0 e
                        ]

distComplexPlus1 :: MemoComplex1 b -> a -> MemoComplex1 (ComplexPlus b a) 
distComplexPlus1 MemoComplex1{..} a = MemoComplex1{ mc1_orig = ComplexPlus mc1_orig a, .. } 

distComplexPlus2 :: MemoComplex2 b -> a -> MemoComplex2 (ComplexPlus b a) 
distComplexPlus2 (MemoComplex2 (ComplexPlus c x)) a = MemoComplex2 (ComplexPlus (distComplexPlus1 c a) x)

distComplexPlus3 :: MemoComplex3 b -> a -> MemoComplex3 (ComplexPlus b a) 
distComplexPlus3 (MemoComplex3 (ComplexPlus c x)) a = MemoComplex3 (ComplexPlus (distComplexPlus2 c a) x)

-- distComplexPlus2 (MemoComplex2 

class Coordinates a where
    coords :: a -> Vert a -> Vec3

instance Coordinates a => Coordinates (MemoComplex1 a) where
    coords = coords . mc1_orig

instance Coordinates a => Coordinates (MemoComplex2 a) where
    coords (MemoComplex2 (ComplexPlus c _)) = coords c

instance Coordinates a => Coordinates (MemoComplex3 a) where
    coords (MemoComplex3 (ComplexPlus c _)) = coords c


instance Coordinates AbstractTet where 
    coords = const (vlbl . fromEnum)
      where
        vlbl 0 = vec3X
        vlbl 1 = vec3Y
        vlbl 2 = vec3Z
        vlbl 3 = 1

instance (v ~ Vert a, Coordinates a) => Coordinates (ComplexPlus (SC3 (BaryVertex3 v)) a) where
    coords (ComplexPlus _ a) v = barycenter (fmap (coords a) v) 

instance (v ~ Vert a, Coordinates a) => Coordinates (ComplexPlus (SC2 (BaryVertex2 v)) a) where
    coords (ComplexPlus _ a) v = barycenter (fmap (coords a) v) 

-- 
--
-- 
-- baryGeom :: (Ord v, Vector vlbl) => SimplicialComplex3 v vlbl -> SimplicialComplex3 (BaryVertex v) vlbl
-- baryGeom s@SimplicialComplex3 {vlbl} = (bary s) { vlbl = barycenter . fmap vlbl }
-- 
--

class Barycenter x where
    barycenter :: x -> Vec3

instance Barycenter (BaryVertex1 Vec3) where
    barycenter (Bary0 v) = v
    barycenter (Bary1 (v0,v1)) = (1/2) *& (v0 &+ v1)

instance Barycenter (BaryVertex2 Vec3) where
    barycenter (BaryVertex1 x) = barycenter x
    barycenter (Bary2 (v0,v1,v2)) = (1/3) *& (v0 &+ v1 &+ v2)

instance Barycenter (BaryVertex3 Vec3) where
    barycenter (BaryVertex2 x) = barycenter x
    barycenter (Bary3 (v0,v1,v2,v3)) = (1/4) *& (v0 &+ v1 &+ v2 &+ v3)

edgeCoords
  :: (Coordinates a, SimplicialComplex1 a) => a -> [Pair Vec3]
edgeCoords c = map2 (coords c) <$> s1 c
triCoords
  :: (Coordinates a, SimplicialComplex2 a) => a -> [Triple Vec3]
triCoords c = map3 (coords c) <$> s2 c


-- instance (SimplicialComplex3 a) => DeltaSet a where
--     type Delta1 a = (Vert a,Vert a)
--     type Delta2 a = (Vert a,Vert a,Vert a)
--     type Delta3 a = (Vert a,Vert a,Vert a,Vert a)
-- 
--     faces32 _ (v0,v1,v2,v3) = ((v1,v2,v3),(v0,v2,v3),(v0,v1,v3),(v0,v1,v2)) 
--     faces21 _ (v0,v1,v2) = ((v1,v2),(v0,v2),(v0,v1))
--     faces10 _ (v0,v1) = (v1,v0)
-- 
--     allTetrahedra = s3


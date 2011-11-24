{-# LANGUAGE TemplateHaskell, TypeOperators, GADTs, FlexibleInstances, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE Rank2Types, UndecidableInstances, NoMonomorphismRestriction, RecordWildCards, CPP, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, PolymorphicComponents, DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonoLocalBinds #-}
{-# OPTIONS -Wall -fwarn-missing-local-sigs #-}
module Simplicial.DeltaSet where


import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Data.Graph.Inductive
import Data.Map as Map
import Data.Maybe
import Data.Typeable
import FaceIx
import HomogenousTuples
import Indexing
import Simplicial.AnySimplex
import Simplicial.StrictlyIncreasingMap
import Test.QuickCheck
import TupleTH
import Util
import qualified Data.Vector as V

-- data RightShift f
-- type instance RightShift f :$ n = f :$ (S n)
-- 
-- data Sequence f = Sequence (f :$ N0) (Sequence (RightShift f)) 
-- 
-- sequenceElem :: Nat n => n -> Sequence f -> f :$ n
-- sequenceElem n = caseNat n
--     (\(Sequence x _) -> x)
--     (\n' (Sequence _ xs) -> sequenceElem n' xs)
-- 
-- toSequence :: forall f. (forall n. Nat n => n -> f :$ n) -> Sequence f
-- toSequence f = Sequence (f n0) (toSequence f')
--     where
--         f' :: forall n. Nat n => n -> RightShift f :$ n
--         f' _ = f (undefined :: S n)







type FaceFunction a  = forall n. Nat n => n -> FaceIx -> (a :$ (S n)) -> (a :$ n)
type SimpsFunction a = forall n. Nat n => n -> [a :$ n]
type SuperFunction a = forall n. Nat n => n -> (a :$ n) -> [a :$ (S n)]
type FaceGraph a = Gr (AnySimplex a) FaceIx
type SimplexNodeMap a = AnySimplex a -> Node

data Dim = 
    -- | Each simplex is contained in a maximal simplex of the given dimension
    HomogenousDim Int |
    -- | The int specifies the maximum dimension
    InhomogenousDim Int
    deriving(Show)




data DeltaSet a = DeltaSet {
    face :: FaceFunction a,
    simps :: SimpsFunction a,
    supers :: SuperFunction a,
    dimension :: Dim,

    -- | Convention: Edges are directed from subfaces to superfaces
    faceGraph :: FaceGraph a,
    nodeMap :: SimplexNodeMap a,
    simpsIndexing :: forall n. Nat n => n -> Indexing (a:$n) 
}

data SimplexNotMemberOfDeltaSet = SimplexNotMemberOfDeltaSet
    deriving(Show,Typeable)

instance Exception SimplexNotMemberOfDeltaSet

nodeMapGet :: Nat n => DeltaSet a -> n -> (a :$ n) -> Node
nodeMapGet a n x = nodeMap a (AnySimplex n x)


-- | Requires 'simps', 'face' and 'dimension' to be defined
mkFaceGraph :: forall a. OrdN a => DeltaSet a -> (FaceGraph a,SimplexNodeMap a)
mkFaceGraph a = (faceGraph',nodeMapFun)
    where
        nodeMap' = Map.fromList (zip (allSimplices a) [0::Node ..]) 

        nodeMapFun x = case Map.lookup x nodeMap' of
                            Just y -> y
                            _ -> throw SimplexNotMemberOfDeltaSet

        faceGraph' :: Gr (AnySimplex a) FaceIx
        faceGraph' =
            mkGraph 
            ((\(x,y) -> (y,x)) <$> Map.assocs nodeMap')
            (do
                (AnySimplex n x, xnode) <- Map.assocs nodeMap'
                caseNat n
                    []
                    (\n' -> do
                        i <- [0..maxFaceIx n]
                        let 
                            y = face a n' i x
                            ynode = nodeMap' ! AnySimplex n' y
                        return (ynode, xnode, i))
            )


data InvalidFaceGraph = InvalidFaceGraph String
    deriving (Show,Typeable)

instance Exception InvalidFaceGraph

-- | Requires 'nodeMap' and 'faceGraph' to be defined
mkSupers :: OrdN a => DeltaSet a -> SuperFunction a
mkSupers a n x = do
    (node,_)  <- lsuc (faceGraph a) (nodeMapGet a n x) 
    let n'' = successor n
        
        
    case fromJust (lab (faceGraph a) node) of
         AnySimplex n' x' -> 
            caseEqNat n' n''
                (return x')
                (throw (InvalidFaceGraph (
                                        "In mkSupers: Expected dimension: "++show n''
                                        ++"; Actual dimension: "++show n')))


type AnyIndexing a = AnySimplex (ApplyConstr Indexing :. a)
                            
mkDeltaSet :: forall a. OrdN a => FaceFunction a -> SimpsFunction a -> Dim -> DeltaSet a                            
mkDeltaSet face_ simps_ dimension_ = r
    where
        r :: DeltaSet a
        r = DeltaSet face_ simps_ (mkSupers r) dimension_ fg nm indexing
        (fg,nm) = mkFaceGraph r 

        indexing :: forall n0. Nat n0 => n0 -> Indexing (a:$n0)
        indexing =
            let
                v = V.fromList (mapDimensions (\n -> 
                        AnySimplex n (getOrd (undefined :: a) n 
                                        (Indexing.fromDistinctList (simps_ n)))
                            :: AnyIndexing a
                        
                            ) 
                                              r)
            in
                \(n::n) -> case v V.! natToInt n of
                           AnySimplex n' is ->
                               caseEqNat n n' 
                                (is :: Indexing (a:$n)) 
                                (error "mkDeltaSet/indexing: Internal error" :: Indexing (a:$n))

mkHomogenousDeltaSet :: forall a n. (Nat n, OrdN a) => 
    n -> FaceFunction a -> [a :$ n] -> DeltaSet a                            
mkHomogenousDeltaSet n face_ topsimps = mkDeltaSet face_ simps_ (HomogenousDim nint)
    where
        nint = natToInt n

--         subsimps_ :: forall k. Nat k => k -> [a:$(S k)] -> [a:$k]
--         subsimps_ k xs = getOrd (undefined :: a) k (
--                             nub' [ face_ k i x | x <- xs, i <- [0..maxFaceIx k] ]) 


        imp :: forall x. x
        imp = error "mkHomogenousDeltaSet: internal error (should be impossible)"

        subsimps_ :: [AnySimplex a] -> [AnySimplex a]
        subsimps_ xs = nub' $ do
                AnySimplex k x <- xs
                caseNat k imp 
                    (\k' -> do
                        i <- [0..maxFaceIx k] 
                        return (AnySimplex k' (face_ k' i x)))
                

        simpsMemo :: [[AnySimplex a]]
        simpsMemo = go 0
            where
                go k | k == nint = fmap (AnySimplex n) topsimps : repeat []
                     | otherwise = 
                        let
                            rec = go (k+1)
                        in
                            subsimps_ (head rec) : rec 



        simps_ :: SimpsFunction a
        simps_ k = do
            AnySimplex k' x <- simpsMemo !! natToInt k
            caseEqNat k k' (return x) imp



type Vert a = a :$ N0
type Arc a =  a :$ N1
type Tri a =  a :$ N2
type Tet a =  a :$ N3


-- data DS s = DS {
--     -- | The first component of the result must be the face obtained by leaving out the first vertex, and so on
--     face_   :: FaceFunction s,
--     simps_  :: SimpsFunction s,
--     supers_ :: SuperFunction s,
--     dimension_ :: Dim,
--     faceGraph_ :: FaceGraph (DS s)
-- }

-- type instance DS a :$ n = a :$ n

-- -- | This is more or less the identity 
-- joinDS :: DS (DS a) -> DS a
-- joinDS DS{..} = DS{..}

-- instance OrdN a => DeltaSet (DS a) where
--     face = face_
--     simps = simps_
--     supers = supers_
--     dimension = dimension_
--     faceGraph = faceGraph_

faces10 :: DeltaSet a -> Arc a -> Pair (Vert a)
faces10 a x = map2 (\i -> face a n0 i x) (0,1)

faces21 :: DeltaSet a -> Tri a -> Triple (Arc a)
faces21 a x = map3 (\i -> face a n1 i x) (0,1,2)

faces32 :: DeltaSet a -> Tet a -> Quadruple (Tri a)
faces32 a x = map4 (\i -> face a n2 i x) (0,1,2,3)

s0 :: DeltaSet a -> [Vert a]
s0 = flip simps n0
s1 :: DeltaSet a -> [Arc a]
s1 = flip simps n1
s2 :: DeltaSet a -> [Tri a]
s2 = flip simps n2
s3 :: DeltaSet a -> [Tet a]
s3 = flip simps n3

super01 :: DeltaSet a -> Vert a -> [Arc a]
super01 = flip supers n0

super12 :: DeltaSet a -> Arc a -> [Tri a]
super12 = flip supers n1

super23 :: DeltaSet a -> Tri a -> [Tet a]
super23 = flip supers n2


faces20 :: DeltaSet a -> Tri a -> (Vert a, Vert a, Vert a)
faces20 t tr = (v2,v1,v0)
    where
        (e12,e02,_) = faces21 t tr
        (v2,v1) = faces10 t e12
        (_,v0) = faces10 t e02

faces20Ascending :: DeltaSet a -> Tri a -> (Vert a, Vert a, Vert a)
faces20Ascending t = $(reverseTuple 3) . faces20 t 

faces31
  :: DeltaSet a
     -> Tet a
     -> (Arc a, Arc a, Arc a, Arc a, Arc a, Arc a)
faces31 t tet = (e23,e13,e12,e03,e02,e01)
    where
        (tr123,tr023,tr013,_) = faces32 t tet
        (e23,e13,e12) = faces21 t tr123
        ( _ ,e03,e02) = faces21 t tr023
        ( _ , _ ,e01) = faces21 t tr013

faces30
  :: DeltaSet a -> Tet a -> (Vert a, Vert a, Vert a, Vert a)
faces30 t tet = (v3,v2,v1,v0)
    where
        (tr123,tr023,_,_) = faces32 t tet
        (v3,v2,v1) = faces20 t tr123
        ( _, _,v0) = faces20 t tr023


-- s0_default t = Set.unions (fmap f (s3 t))
--     where
--         f = Set.fromList . $(tupleToList 4) . faces30 t
-- 
-- s1_default t = Set.unions (fmap f (s3 t))
--     where
--         f = Set.fromList . $(tupleToList 6) . faces31 t
-- 
-- s2_default t = Set.unions (fmap f (s3 t))
--     where
--         f = Set.fromList . $(tupleToList 4) . faces32 t






memoFun :: Ord k => (k -> a) -> [k] -> Map k a
memoFun f xs = Map.fromList [(x,f x) | x <- xs] 

memoSuper
  :: forall a. (Ord (Vert a), Ord (Arc a), Ord (Tri a)) => DeltaSet a -> DeltaSet a
memoSuper d = 
    let
            super01_ = (memoFun (super01 d) (s0 d) !)
            super12_ = (memoFun (super12 d) (s1 d) !)
            super23_ = (memoFun (super23 d) (s2 d) !)

            supers_ :: SuperFunction a
            supers_ n = caseNat3 n
                            super01_
                            super12_
                            super23_
                            (const $ error ("'supers' called at dimension > 2 for a memoSuper'd complex"))

    in
        --DeltaSet (faceFunction d) (simps d) supers_ (dimension d) (faceGraph d)
        d { supers = supers_ }
    

instance (ShowN s) => Show (DeltaSet s) where
    showsPrec prec a = showsPrec prec (faceGraph a) 

--                     concat [ 
--                         f (s3 a) (asList . faces32 a), 
--                         f (s2 a) (asList . faces21 a), 
--                         f (s1 a) (asList . faces10 a),
--                         showVerts
--                         ]
--         where
--             f :: (Show a, Show b) => [a] -> (a -> [b]) -> String
--             f cs _faces = unlines $ do
--                 c <- cs
--                 (getShow  show c ++ " -> ") : do
--                     b <- _faces c
--                     ["    " ++ show b]
-- 
--             showVerts | HomogenousDim n <- dimension a, n > 0 = ""
--                       | otherwise = 
--                             unlines (fmap show (s0 a)) 

data FaceIndexOutOfBounds = FaceIndexOutOfBounds FaceIx deriving(Show,Typeable)
instance Exception FaceIndexOutOfBounds
                    

        

dimMax :: Dim -> Int
dimMax d = case d of
                      HomogenousDim n -> n
                      InhomogenousDim n -> n

maxDimension :: DeltaSet a -> Int
maxDimension = dimMax . dimension



            
simpleMapAnySimplex :: (forall n. n -> a:$n -> b:$n) -> AnySimplex a -> AnySimplex b
simpleMapAnySimplex f (AnySimplex n a) = AnySimplex n (f n a)
        

-- foldMapDJ
--   :: (t3 -> t4 -> t)
--      -> (t1 -> t3) -> (t2 -> t4) -> DisjointUnion t1 t2 -> t
-- foldMapDJ g f f' (DisjointUnion a b) = g (f a) (f' b)
-- 
-- deriveDJ :: (a -> a' -> r) -> (b -> b' -> r) -> DisjointUnion a b -> Either a' b' -> r
-- deriveDJ = foldMapDJ (|||)


data ComplexPlus a b = ComplexPlus {
    cp_complex :: a,
    cp_extra :: b
}
    deriving Show

-- DERIVE_DELTA_SET(ComplexPlus a b, cp_complex) 






            

    

mapDimensions :: forall a r. (forall n. Nat n => n -> r) -> DeltaSet a -> [r]
mapDimensions f a = go (maxDimension a) n0
    where
        go :: Nat n => Int -> n -> [r]
        go i n | i < 0 = []
               | otherwise = f n : go (i-1) (successor n)

allSimplices :: DeltaSet a -> [AnySimplex a]
allSimplices a = concat $ mapDimensions (\n -> AnySimplex n <$> simps a n) a 

maxFaceIx :: Nat n => n -> FaceIx
maxFaceIx = FI . natToInt

faces :: (Nat n) => DeltaSet a -> n -> (a :$ S n) -> [a :$ n]
faces a n x = [ face a n i x | i <- [0..maxFaceIx n] ] 



checkFaceOrderConsistency :: forall a m. (OrdN a, ShowN a, m ~ Either String) => 
    DeltaSet a -> m ()
checkFaceOrderConsistency ds = (sequence_ :: [m()] -> m()) $ do -- list monad
    AnySimplex n x <- allSimplices ds
    caseNat2 n [return ()] [return ()]
        (\n'' -> do
            let n' = successor n''
            i <- [0..maxFaceIx n']
            j <- [i+1..maxFaceIx n] 
            let d_ij_x = face ds n'' i (face ds n' j x) 
            let d_ji_x = face ds n'' (j-1) (face ds n' i x) 
            [
              unless (getOrd (undefined :: a) n'' (d_ij_x == d_ji_x))
                (throwError $ getShow (undefined :: a) n'' $ getShow (undefined :: a) n $
                    concat ["i = ",show i,"; j = ",show j,"; x = ",show x,
                    "; d_i (d_j x) = ",show d_ij_x,
                    "; d_{j-1} (d_i x) = ",show d_ji_x])

             ]

        )





oneSkeleton :: DeltaSet a -> Gr (Vert a) (Arc a)
oneSkeleton ds = mkGraph 
    (do
        v <- simps ds n0
        return (nodeMapGet ds n0 v,v))
    (do
        e <- simps ds n1
        let (nod,nod') = nodeMapGet ds n0 `map2` faces10 ds e
        return (nod,nod',e)) 


anySimplex_face :: DeltaSet a -> FaceIx -> AnySimplex a -> AnySimplex a
anySimplex_face ds i (AnySimplex n x) =
    caseNat n 
        (error "anySimplex_face: Tried to take a face of a 0-simplex")
        (\n' -> AnySimplex n' (face ds n' i x))

ds_fmap :: forall a. DeltaSet a -> StrictlyIncreasingMap -> AnySimplex a -> AnySimplex a 
ds_fmap ds sim = loop (decomposeToCofaceMaps sim)
    where
        loop [] = id
        loop (i:is) = anySimplex_face ds i . loop is


prop_ds_fmap_consistent_with_face
  :: (ShowN a, OrdN a) => DeltaSet a -> Property
prop_ds_fmap_consistent_with_face ds =
    forAll (elements (allSimplices ds) `suchThat` ((>0) . anySimplex_dim)) (\anys ->
        let n = anySimplex_dim anys
        in
            forAll (genFaceIx n) (\i ->
                ds_fmap ds (cofaceMap n i) anys == anySimplex_face ds i anys))

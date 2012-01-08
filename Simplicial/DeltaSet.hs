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
import Data.Proxy
import Data.Typeable
import FaceClasses as F
import FaceIx
import HomogenousTuples
import Numbering
import PrettyUtil
import Simplicial.AnySimplex
import Simplicial.DeltaSet3
import Simplicial.StrictlyIncreasingMap
import Test.QuickCheck
import TupleTH
import Util
import qualified Data.Vector as V

-- data RightShift f
-- type instance RightShift f  n = f  (S n)
-- 
-- data Sequence f = Sequence (f  N0) (Sequence (RightShift f)) 
-- 
-- sequenceElem :: Nat n => n -> Sequence f -> f  n
-- sequenceElem n = caseNat n
--     (\(Sequence x _) -> x)
--     (\n' (Sequence _ xs) -> sequenceElem n' xs)
-- 
-- toSequence :: forall f. (forall n. Nat n => n -> f  n) -> Sequence f
-- toSequence f = Sequence (f n0) (toSequence f')
--     where
--         f' :: forall n. Nat n => n -> RightShift f  n
--         f' _ = f (undefined :: S n)







type FaceFunction a  = forall n. Nat n => FaceIx -> a (S n) -> a  n
type SimpsFunction a = forall n. Nat n => n -> [a  n]
type SuperFunction a = forall n. Nat n => a n -> [a (S n)]
type FaceGraph a = Gr (AnySimplex a) FaceIx
type SimplexNodeMap a = AnySimplex a -> Node

-- | Invariant: Dimensions are nonnegative, 'InhomogenousDim 0' is an invalid value (since, if 0 is the maximal dimension of simplices, then the DeltaSet is homogenous)
data Dim = 
    -- | Each simplex is contained in a maximal simplex of the given dimension
    HomogenousDim Int |
    -- | The int specifies the maximum dimension.
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
    simpsIndexing :: forall n. Nat n => Numbering (a n) 
}

data SimplexNotMemberOfDeltaSet = SimplexNotMemberOfDeltaSet
    deriving(Show,Typeable)

instance Exception SimplexNotMemberOfDeltaSet

nodeMapGet :: Nat n => DeltaSet a -> (a n) -> Node
nodeMapGet a x = nodeMap a (AnySimplex x)


-- | Requires 'simps', 'face' and 'dimension' to be defined
mkFaceGraphAndSimplexNodeMap :: forall a. OrdN a =>  FaceFunction a -> SimpsFunction a -> Dim
    -> (FaceGraph a,SimplexNodeMap a)
mkFaceGraphAndSimplexNodeMap face_ simps_ dimension_ = 
            (faceGraph_,nodeMapFun)
    where
        nodeMap_ = Map.fromList (zip (allSimplices0 (dimMax dimension_) simps_) [0::Node ..]) 

        faceGraph_ = mkFaceGraph face_ nodeMap_

        nodeMapFun x = case Map.lookup x nodeMap_ of
                            Just y -> y
                            _ -> throw SimplexNotMemberOfDeltaSet

mkFaceGraph ::
 forall a. OrdN a =>  FaceFunction a -> Map (AnySimplex a) Node -> FaceGraph a

mkFaceGraph face_ nodeMap_ =
            mkGraph 
            ((\(x,y) -> (y,x)) <$> Map.assocs nodeMap_)
            (do
                (asi, xnode) <- Map.assocs nodeMap_
                elimAnySimplexWithNat asi (\n x ->
                    caseNat n
                        []
                        (\(_ :: predn) -> do
                            i <- [0..maxFaceIx n]
                            let 
                                y = face_ i x :: a predn
                                ynode = nodeMap_ ! AnySimplex y
                            return (ynode, xnode, i)))
            )


data InvalidFaceGraph = InvalidFaceGraph String
    deriving (Show,Typeable)

instance Exception InvalidFaceGraph

-- | Requires 'nodeMap' and 'faceGraph' to be defined
mkSupers :: forall a. OrdN a => SimplexNodeMap a -> FaceGraph a -> SuperFunction a
mkSupers nodeMap_ faceGraph_ (x :: a n) = do
    (node,_)  <- lsuc faceGraph_ (nodeMap_ (AnySimplex x)) 
    let 
        n = undefined :: n
        n'' = successor n
        
        
    elimAnySimplexWithNat (fromJust (lab faceGraph_ node)) 
        (\n' x' ->
                caseEqNat n' n''
                    (return x')
                    (throw (InvalidFaceGraph (
                                            "In mkSupers: Expected dimension: "++show n''
                                            ++"; Actual dimension: "++show n'))))


data AnyIndexing a = forall n. Nat n => AnyIndexing (Numbering (a n))
                            
mkDeltaSet :: forall a. OrdN a => FaceFunction a -> SimpsFunction a -> Dim -> DeltaSet a                            
mkDeltaSet face_ simps_ dimension_ = r
    where
        r :: DeltaSet a
        r = DeltaSet face_ simps_ (mkSupers nm fg) dimension_ fg nm indexing

        (fg,nm) = mkFaceGraphAndSimplexNodeMap face_ simps_ dimension_ 


        indexingsVector = V.fromList (mapDimensions0 (\(_ :: n) -> 
                        AnyIndexing (getOrd (undefined :: Proxy (a n)) 
                                        (Numbering.fromDistinctList (simps_ undefined :: [a n])))
                            :: AnyIndexing a
                        
                            ) 
                                              (dimMax dimension_))

        indexing :: forall n. Nat n => Numbering (a n)
        indexing =
            let
                n = (undefined :: n)
            in
                case indexingsVector V.! natToInt n of
                           AnyIndexing (is :: Numbering (a n')) ->
                               caseEqNat n (undefined :: n') 
                                (is :: Numbering (a n)) 
                                (error "mkDeltaSet/indexing: Internal error" :: Numbering (a n))

mkHomogenousDeltaSet :: forall a n. (Nat n, OrdN a) => 
    FaceFunction a -> [a n] -> DeltaSet a                            
mkHomogenousDeltaSet face_ topsimps = mkDeltaSet face_ simps_ (HomogenousDim nint)
    where
        n = undefined :: n
        nint = natToInt n

--         subsimps_ :: forall k. Nat k => k -> [a(S k)] -> [ak]
--         subsimps_ k xs = getOrd (undefined :: a) k (
--                             nub' [ face_ k i x | x <- xs, i <- [0..maxFaceIx k] ]) 


        imp :: forall x. x
        imp = error "mkHomogenousDeltaSet: internal error (should be impossible)"

        subsimps_ :: [AnySimplex a] -> [AnySimplex a]
        subsimps_ asis = nub' $ do
                asi <- asis
                elimAnySimplexWithNat asi (\k x ->
                    caseNat k imp 
                        (\_ -> do
                            i <- [0..maxFaceIx k] 
                            return (AnySimplex (face_ i x))))
                

        simpsMemo :: [[AnySimplex a]]
        simpsMemo = go 0
            where
                go k | k == nint = fmap AnySimplex topsimps : repeat []
                     | otherwise = 
                        let
                            rec = go (k+1)
                        in
                            subsimps_ (head rec) : rec 



        simps_ :: SimpsFunction a
        simps_ = mkSimpsFunction (\(k :: k) -> do
            asi <- simpsMemo !! natToInt k
            elimAnySimplexWithNat asi (\k' x ->
                caseEqNat k k' (return x) imp))


mkSimpsFunction :: (forall n. Nat n => n -> [a n]) -> SimpsFunction a
mkSimpsFunction f = f

mkFaceFunctionWithNat :: (forall n. Nat n => n -> FaceIx -> a (S n) -> a n) -> FaceFunction a
mkFaceFunctionWithNat f = f undefined



mkTuply2dFaceFunc :: (a N1 -> Pair (a N0)) -> (a N2 -> Triple (a N1)) -> FaceFunction a 
mkTuply2dFaceFunc f0 f1 = 
    mkFaceFunctionWithNat (\n i -> 
        caseNat2 n
            (case i of
                        0 -> fst . f0
                        1 -> snd . f0
                        _ -> throw (FaceIndexOutOfBounds i))
            (case i of
                        0 -> $(proj 3 0) . f1
                        1 -> $(proj 3 1) . f1
                        2 -> $(proj 3 2) . f1
                        _ -> throw (FaceIndexOutOfBounds i))
            (\_ _ -> error "mkTuply2dFaceFunc: dimension > 2"))

-- data DS s = DS {
--     -- | The first component of the result must be the face obtained by leaving out the first vertex, and so on
--     face_   :: FaceFunction s,
--     simps_  :: SimpsFunction s,
--     supers_ :: SuperFunction s,
--     dimension_ :: Dim,
--     faceGraph_ :: FaceGraph (DS s)
-- }

-- type instance DS a  n = a  n

-- -- | This is more or less the identity 
-- joinDS :: DS (DS a) -> DS a
-- joinDS DS{..} = DS{..}

-- instance OrdN a => DeltaSet (DS a) where
--     face = face_
--     simps = simps_
--     supers = supers_
--     dimension = dimension_
--     faceGraph = faceGraph_

instance Vertices (DeltaSet a) where
    type Verts (DeltaSet a) = [a N0]
    vertices = simps'

instance Edges (DeltaSet a) where
    type Eds (DeltaSet a) = [a N1]
    edges = simps'

instance Triangles (DeltaSet a) where
    type Tris (DeltaSet a) = [a N2]
    triangles = simps'

instance Tetrahedra (DeltaSet a) where
    type Tets (DeltaSet a) = [a N3]
    tetrahedra = simps'

instance DeltaSet1 (DeltaSet a) where 
    faces10 a x = map2 (\i -> face a i x) (0,1)

instance DeltaSet2 (DeltaSet a) where 
    faces21 a x = map3 (\i -> face a i x) (0,1,2)

instance DeltaSet3 (DeltaSet a) where
    faces32 a x = map4 (\i -> face a i x) (0,1,2,3)

simps' :: Nat n => DeltaSet a -> [a n]
simps' ds = simps ds undefined 


super01 :: DeltaSet a -> a N0 -> [a N1]
super01 = supers

super12 :: DeltaSet a -> a N1 -> [a N2]
super12 = supers

super23 :: DeltaSet a -> a N2 -> [a N3]
super23 = supers






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
  :: forall a. (Ord (a N0), Ord (a N1), Ord (a N2)) => DeltaSet a -> DeltaSet a
memoSuper d = 
    let
            super01_ = (memoFun (super01 d) (vertices d) !)
            super12_ = (memoFun (super12 d) (F.edges d) !)
            super23_ = (memoFun (super23 d) (triangles d) !)

            supers_ :: SuperFunction a
            supers_ (x :: a n) = 
                caseNat3 (undefined :: n)
                            (super01_ x)
                            (super12_ x)
                            (super23_ x)
                            (const $ error ("'supers' called at dimension > 2 for a memoSuper'd complex"))

    in
        --DeltaSet (faceFunction d) (simps d) supers_ (dimension d) (faceGraph d)
        d { supers = supers_ }
    

instance (ShowN s) => Show (DeltaSet s) where
    showsPrec = prettyShowsPrec

instance (ShowN s) => Pretty (DeltaSet s) where
    pretty ds = 
        prettyRecord "DeltaSet"
           [
            ("nodeMap", prettyFunction (nodeMap ds) (allSimplices ds)),
            ("faceGraph", string (show (faceGraph ds)))
            
           ]
        

--                     concat [ 
--                         f (s3 a) (asList . faces32 a), 
--                         f (triangles a) (asList . faces21 a), 
--                         f (edges a) (asList . faces10 a),
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





mapDimensions0 :: forall r. (forall n. Nat n => n -> r) -> Int -> [r]
mapDimensions0 f maxDimension_  = go maxDimension_ n0
    where
        go :: Nat n => Int -> n -> [r]
        go i n | i < 0 = []
               | otherwise = f n : go (i-1) (successor n)
    

mapDimensions :: forall a r. (forall n. Nat n => n -> r) -> DeltaSet a -> [r]
mapDimensions f a = mapDimensions0 f (maxDimension a)


allSimplices0 :: forall a. Int -> SimpsFunction a -> [AnySimplex a]
allSimplices0 maxDim simps_ = 
    concat $ mapDimensions0 (\(_ :: n) -> AnySimplex <$> (simps_ undefined :: [a n])) maxDim 

allSimplices :: forall a. DeltaSet a -> [AnySimplex a]
allSimplices a = allSimplices0 (maxDimension a) (simps a)

maxFaceIx :: Nat n => n -> FaceIx
maxFaceIx = FI . natToInt

faces :: forall n a. (Nat n) => DeltaSet a -> a (S n) -> [a n]
faces a x = [ face a i x | i <- [0..maxFaceIx (undefined :: n)] ] 



checkFaceOrderConsistency :: forall a m. (OrdN a, ShowN a, m ~ Either String) => 
    DeltaSet a -> m ()
checkFaceOrderConsistency ds = (sequence_ :: [m()] -> m()) $ do -- list monad
  asi <- allSimplices ds
  elimAnySimplexWithNat asi (\(n :: n) x ->
    caseNat2 n [return ()] [return ()]
        (\(n'' :: n'') -> do
            let n' = successor n''
            i <- [0..maxFaceIx n']
            j <- [i+1..maxFaceIx n] 
            let d_ij_x = face ds i (face ds j x) :: a n'' 
            let d_ji_x = face ds (j-1) (face ds i x) :: a n''
            let pro = undefined :: Proxy (a n'')
            [
              unless (getOrd pro  (d_ij_x == d_ji_x))
                (throwError $ getShow pro $ getShow (undefined :: Proxy (a n)) $
                    concat ["i = ",show i,"; j = ",show j,"; x = ",show x,
                    "; d_i (d_j x) = ",show d_ij_x,
                    "; d_{j-1} (d_i x) = ",show d_ji_x])

             ]

        ))





oneSkeletonGraph :: DeltaSet a -> Gr (a N0) (a N1)
oneSkeletonGraph ds = mkGraph 
    (do
        v <- vertices ds
        return (nodeMapGet ds v,v))
    (do
        e <- F.edges ds
        let (nod,nod') = nodeMapGet ds `map2` faces10 ds e
        return (nod,nod',e)) 




instance OrdN a => OneSkeletonable (DeltaSet a) where
    oneSkeleton ds =
        mkDeltaSet 
            (face ds) 
            (\n -> caseNat2 n
                    (vertices ds)
                    (F.edges ds)
                    (const []))

            (case dimension ds of
                HomogenousDim 0 -> HomogenousDim 0
                HomogenousDim _ -> HomogenousDim 1
                InhomogenousDim _ -> InhomogenousDim 1)



anySimplex_face :: DeltaSet a -> FaceIx -> AnySimplex a -> AnySimplex a
anySimplex_face ds i asi = elimAnySimplexWithNat asi (\n x ->
    caseNat n 
        (error "anySimplex_face: Tried to take a face of a 0-simplex")
        (\_ -> AnySimplex (face ds i x)))

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


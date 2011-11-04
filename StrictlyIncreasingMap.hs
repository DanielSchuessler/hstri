{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wall #-}
module StrictlyIncreasingMap where
import Test.QuickCheck
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Control.Exception
import Data.Sequence as Seq
import qualified Data.Foldable as Fold
import Control.Applicative
import Data.List as List
import Data.Semigroup
import qualified Data.List.NonEmpty as NonEmpty
import FaceIx


-- | @StrictlyIncreasingMap dom cod im@ is the injective, monotonous finite map from @{0, .., dom}@ to @{0, .., cod}@ with image @im@
data StrictlyIncreasingMap = StrictlyIncreasingMap Int Int [Int]
    deriving (Show,Eq,Ord)

sim_dom :: StrictlyIncreasingMap -> Int
sim_dom (StrictlyIncreasingMap dom _ _) = dom
sim_cod :: StrictlyIncreasingMap -> Int
sim_cod (StrictlyIncreasingMap _ cod _) = cod
sim_image :: StrictlyIncreasingMap -> [Int]
sim_image (StrictlyIncreasingMap _ _ im) = im


strictlyIncreasingMap :: Int -> Int -> [Int] -> StrictlyIncreasingMap
strictlyIncreasingMap _dom cod im =
                    assert (List.length im == _dom+1) $
                    assert (isStrictlyIncreasing im) $
                    assert (_dom == -1 || (im/=[] && head im >= 0 && last im <= cod)) $ 
                    result
    where
        result = 
            StrictlyIncreasingMap _dom cod im


seqDeleteAt :: Show a => Int -> Seq a -> Seq a
seqDeleteAt i xs = case Seq.splitAt i xs of
                        (l, viewl -> _ :< r) -> l Seq.>< r
                        _ -> error (unwords ["seqDeleteAt",show i,show xs])


genStrictlyIncreasingMap :: Int -> Int -> Gen StrictlyIncreasingMap
genStrictlyIncreasingMap _dom cod = do
        im <- 
            let
                loop xs 0 = return xs
                loop xs k = do
                        i <- choose (0,Seq.length xs-1)
                        loop (seqDeleteAt i xs) (k-1)

        
        
            in Fold.toList <$> loop (Seq.fromList [0..cod]) (cod - _dom) 

        return $ strictlyIncreasingMap _dom cod im

genSmallInt :: Int -> Gen Int
genSmallInt _min = sized (\n -> choose (_min,_min+n))

instance Arbitrary StrictlyIncreasingMap where
    arbitrary = do
        _dom <- genSmallInt (-1)
        cod <- genSmallInt _dom
        genStrictlyIncreasingMap _dom cod



    



instance Semigroup StrictlyIncreasingMap where
    (<>) f2@(StrictlyIncreasingMap dom2 cod2 im2) 
            f1@(StrictlyIncreasingMap dom1 cod1 im1) =
            

                if (cod1==dom2) 
                then strictlyIncreasingMap dom1 cod2 ((themap2 IntMap.!) <$> im1)
                else (error (unwords ["Not composable: ",show f2, "<>", show f1]))

        where
            themap2 = IntMap.fromAscList (Prelude.zip [0..] im2)


isStrictlyIncreasing :: [Int] -> Bool
isStrictlyIncreasing [] = True
isStrictlyIncreasing [_] = True
isStrictlyIncreasing (x1:(xs@(x2:_))) = x1 < x2 && isStrictlyIncreasing xs 

cofaceMap :: Int -> FaceIx -> StrictlyIncreasingMap
cofaceMap n (runFI -> i) = 
    if (i<0 || i > n) 
       then error (unwords ["cofaceMap",show n, show i])
       else StrictlyIncreasingMap (n-1) n ([0..i-1]++[i+1..n])

prop_cofaceMap_relations :: Property
prop_cofaceMap_relations = 
    forAll (genSmallInt 1) (\n ->
    forAll (choose (0,FI n-1)) (\i ->
    forAll (choose (i+1,FI n)) (\j ->
        let l = (cofaceMap n j) <> (cofaceMap (n-1) i)
            r = (cofaceMap n i) <> (cofaceMap (n-1) (j-1))
        in
            l == r)))
            
funToStrictlyIncreasingMap :: Int -> Int -> (Int -> Int) -> StrictlyIncreasingMap
funToStrictlyIncreasingMap _dom cod f =
    strictlyIncreasingMap _dom cod (f <$> [0.._dom])

idStrictlyIncreasingMap :: Int -> StrictlyIncreasingMap
idStrictlyIncreasingMap n = strictlyIncreasingMap n n [0..n] 

sim_apply :: StrictlyIncreasingMap -> Int -> Int
sim_apply (StrictlyIncreasingMap _dom _ im) i = im !! i

decomposeToCofaceMaps :: StrictlyIncreasingMap -> [FaceIx]
decomposeToCofaceMaps (StrictlyIncreasingMap _dom cod im) =
    FI <$> List.filter (\i -> not (i `IntSet.member` imSet)) [cod,cod-1 .. 0] 
  where
    imSet = IntSet.fromDistinctAscList im


prop_decomposeToCofaceMaps :: StrictlyIncreasingMap -> Bool
prop_decomposeToCofaceMaps s = sim_dom s == sim_cod s || s == sconcat (NonEmpty.fromList cofaceMaps)
    where
        cofaceMaps = List.zipWith cofaceMap [sim_cod s, sim_cod s - 1  ..] (decomposeToCofaceMaps s)




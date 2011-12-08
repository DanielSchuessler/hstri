{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}
module Collections where
import qualified Data.Set as S
import qualified Data.Map as M
import Language.Haskell.TH
import Test.QuickCheck
import Data.List as L


type Set = S.Set
type Map = M.Map

setFromList = S.fromList
setToList = S.toList
setToAscList = S.toAscList
member = S.member
mapSet = S.map
mapSetMonotonic = S.mapMonotonic
singletonSet = S.singleton
setSize = S.size
emptySet = S.empty
setUnion = S.union
elemOfSetAt i xs | i < S.size xs = (!! i) . setToList $ xs
                 | otherwise = error ("elemOfSetAt "++show i++" "++showsPrec 11 xs "")

mapFromList = M.fromList
(!) = (M.!)
memberOfMap = M.member
mapInsert = M.insert
keys = M.keys
emptyMap = M.empty
elems = M.elems
lookup = M.lookup
fromListWithKey = M.fromListWithKey
intersectionWith = M.intersectionWith
singletonMap = M.singleton

deriveCollectionKeyClass :: Name -> DecsQ
deriveCollectionKeyClass _ = return [] 



class Sized xs where
    size :: xs -> Int

class DeleteAt xs where
    deleteAt :: Int -> xs -> xs


instance Sized [a] where size = length
instance Sized (S.Set a) where size = S.size                         

instance DeleteAt [a] where
    deleteAt i xs = case splitAt i xs of
                           (xs1,[]) -> xs1
                           (xs1,_:xs2) -> xs1 ++ xs2

instance Ord a => DeleteAt (S.Set a) where
    deleteAt i = S.fromDistinctAscList . deleteAt i . S.toAscList
    
    


subsetsSized :: (DeleteAt t, Sized t) => Int -> t -> [t]
subsetsSized k s =
    let
        n = size s
        
        go 0 _ s' = [ s' ]
        -- d is the number of elements left to delete
        go d minIx s' = [ deleteAt i s''
                          | i <- [minIx .. (n-1)]
                          , s'' <- go (d-1) (i+1) s' ]
    in
        go (n-k) 0 s


hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length xs /= setSize (setFromList xs)

unionMaybeWith
  :: Ord k =>
     (b -> b -> Maybe b) -> M.Map k b -> M.Map k b -> M.Map k b

unionMaybeWith f xs ys = M.mapMaybe id $ M.unionWith f' xs' ys'
    where
        xs' = M.map Just xs
        ys' = M.map Just ys
        f' (Just x) (Just y) = f x y



-- instance TKey a => SetLike (TSet a) where
--     slSize = Set.size
--     slDeleteAt = Set.deleteAt
-- type instance Element (TSet a) = a
-- 
-- instance TKey a => AsList (TSet a) where 
--     asList = Set.toList
-- 
-- 
-- instance (TKey a, Arbitrary a) => Arbitrary (TSet a) where 
--     arbitrary = Set.fromList `fmap` arbitrary
--     shrink = fmap Set.fromList . shrink . Set.toList
-- 
-- 
-- genTMap :: (TKey k) =>Gen [k] -> Gen a -> Gen (TMap k a)
-- genTMap genKey genElem = do
--     keys_ <- Set.fromList `fmap` genKey
--     elems_ <- vectorOf (Set.size keys_) genElem
--     return (Map.fromAscList (zip (Set.toList keys_) elems_))
-- 
-- 
-- instance (TKey a, Arbitrary a, Arbitrary v) => Arbitrary (TMap a v) where
--     arbitrary = genTMap arbitrary arbitrary
--     shrink = fmap Map.fromList . shrink . Map.assocs
-- 
-- prop_distinctUnorderedPairs ::  TSet Int -> Bool
-- prop_distinctUnorderedPairs (xs :: TSet Int) = normalize expected == normalize (distinctUnorderedPairs xs)
--     where
--         normalize = List.sort . fmap normalizePair
--         normalizePair (x,y) = case x `compare` y of
--                                    LT -> (x,y)
--                                    EQ -> error ("Pair with two equal elements: "++show(x,y))
--                                    GT -> (y,x)
-- 
--         expected = [ (x,y) | x <- Set.toList xs, y <- Set.toList xs, x < y ]
-- 
-- 
--     
-- hasDuplicates :: (TKey a) => [a] -> Bool
-- hasDuplicates xs = length xs /= Set.size (Set.fromList xs)
-- 
--
-- 
-- -- Internals copied from the TrieMap package
-- 
-- -- | A default implementation of @'RepList' a@.
-- type DRepList a = Vector.Vector (Rep a)
-- 
-- -- | A default implementation of 'toRepList'.
-- dToRepList :: Repr a => [a] -> DRepList a
-- dToRepList = Vector.fromList . Prelude.map toRep
-- 
-- instance (TKey a, Pretty a) => Pretty (TSet a) where
--     pretty = prettyListAsSet . Set.toList 
-- 
-- 
-- instance (TKey a, Pretty a, Pretty b) => Pretty (TMap a b) where
--     pretty = prettyAssocs . Map.assocs 
-- 
-- instance (TKey a) => Repr (TSet a) where
--     type Rep (TSet a) = RepList a
--     type RepList (TSet a) = Vector (RepList a)
-- 
--     toRep = toRep . Set.toAscList
--     toRepList = dToRepList

 

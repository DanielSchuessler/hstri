{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, BangPatterns #-}
module S3 where
import Data.Monoid
import Data.Maybe


-- | Symmetric group on 3 elements
data S3 = S3abc | S3bca | S3cab | S3acb | S3bac | S3cba deriving(Show,Eq,Enum,Bounded)

foldl_s3 ::  (t -> S3 -> t) -> t -> t
foldl_s3 c z = z `c` S3abc `c` S3bca `c` S3cab `c` S3acb `c` S3bac `c` S3cba

foldr_s3 c z = S3abc `c` ( S3bca `c` ( S3cab `c` ( S3acb `c` ( S3bac `c` ( S3cba `c` z )))))


data Triple t = Triple !t !t !t deriving (Show,Eq)

act ::  S3 -> Triple t -> Triple t
act g = case g of
                       S3abc -> \(Triple a b c) -> Triple a b c
                       S3bca -> \(Triple a b c) -> Triple b c a
                       S3cab -> \(Triple a b c) -> Triple c a b
                       S3acb -> \(Triple a b c) -> Triple a c b
                       S3bac -> \(Triple a b c) -> Triple b a c
                       S3cba -> \(Triple a b c) -> Triple c b a

-- {-# INLINE cpsAct #-}
-- cpsAct :: S3 -> CpsTrip t r -> CpsTrip t r
-- cpsAct g t k =  case g of
--                        S3abc -> t (\a b c ->k a b c)
--                        S3bca -> t (\a b c ->k b c a)
--                        S3cab -> t (\a b c ->k c a b)
--                        S3acb -> t (\a b c ->k a c b)
--                        S3bac -> t (\a b c ->k b a c)
--                        S3cba -> t (\a b c ->k c b a)


data ABC = ABC_A | ABC_B | ABC_C deriving(Eq)
abc = Triple ABC_A ABC_B ABC_C

{-# INLINE unAct #-}
unAct f = case f abc of
                       Triple ABC_A ABC_B ABC_C -> S3abc
                       Triple ABC_B ABC_C ABC_A -> S3bca
                       Triple ABC_C ABC_A ABC_B -> S3cab
                       Triple ABC_A ABC_C ABC_B -> S3acb
                       Triple ABC_B ABC_A ABC_C -> S3bac
                       Triple ABC_C ABC_B ABC_A -> S3cba

unAct' f = case f abc of
                       Triple ABC_A ABC_B _ -> S3abc
                       Triple ABC_A _ _ -> S3acb
                       Triple ABC_B ABC_C _ -> S3bca
                       Triple ABC_B _ _ -> S3bac
                       Triple ABC_C ABC_A _ -> S3cab
                       Triple ABC_C _ _ -> S3cba

{-# INLINE unAct'' #-}
unAct'' f = s3the (\g ->  act g abc === y)
    where 
        y = f abc
        Triple a b c === Triple a' b' c' = a==a' && b==b' 



-- cpsABC :: CpsTrip ABC r
-- cpsABC k = k ABC_A ABC_B ABC_C 

-- cpsUnAct f = s3the (\g -> cpsAct g cpsABC (\a b c -> f cpsABC (\a' b' c' -> a==a' && b==b')))


s3mult ::  S3 -> S3 -> S3
s3mult g2 g1 = unAct (act g2 . act g1)

s3mult' ::  S3 -> S3 -> S3
s3mult' g2 g1 = unAct' (act g2 . act g1)

s3mult'' ::  S3 -> S3 -> S3
s3mult'' g2 g1 = unAct'' (act g2 . act g1)

-- s3mult''' ::  S3 -> S3 -> S3
-- s3mult''' g2 g1 = cpsUnAct (cpsAct g2 . cpsAct g1)

s3inv g = s3the (\g' -> s3mult' g' g == S3abc)

s3the p 
    | p S3abc = S3abc
    | p S3acb = S3acb
    | p S3bac = S3bac
    | p S3bca = S3bca
    | p S3cba = S3cba
    | p S3cab = S3cab
    | otherwise = error "s3the_error"



-- instance Monoid S3 where
--     mempty = S3abc
--     mappend = s3mult
-- 
-- 
-- --{-# INLINE staticCases #-}
staticCases ::  (S3 -> t) -> (S3 -> t)
staticCases f x = case x of
                       S3abc -> f S3abc
                       S3bca -> f S3bca
                       S3cab -> f S3cab
                       S3acb -> f S3acb
                       S3bac -> f S3bac
                       S3cba -> f S3cba
-- 
-- 
-- test1 x = s3mult S3abc x
-- test2 x = s3mult S3bca x
-- test3 = staticCases (\x -> s3mult x x)

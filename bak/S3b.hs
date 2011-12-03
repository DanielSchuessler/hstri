{-# LANGUAGE Rank2Types, NoMonomorphismRestriction, BangPatterns #-}
module S3 where
import Data.Monoid
import Data.Maybe


-- | Symmetric group on 3 elements
data S3 = S3abc | S3bca | S3cab | S3acb | S3bac | S3cba deriving(Show,Eq,Enum,Bounded,Ord)


foldl_s3 ::  (t -> S3 -> t) -> t -> t
foldl_s3 c z = z `c` S3abc `c` S3bca `c` S3cab `c` S3acb `c` S3bac `c` S3cba

foldr_s3 c z = S3abc `c` ( S3bca `c` ( S3cab `c` ( S3acb `c` ( S3bac `c` ( S3cba `c` z )))))

s3 = foldr_s3 (:) []


data Triple t = Triple !t !t !t deriving (Show,Eq)


cpsAct ::  S3 -> t -> t -> t -> (t -> t -> t -> r) -> r
cpsAct g a b c k = case g of
                       S3abc -> k a b c
                       S3bca -> k b c a
                       S3cab -> k c a b
                       S3acb -> k a c b
                       S3bac -> k b a c
                       S3cba -> k c b a


act ::  S3 -> (t, t, t) -> (t, t, t)
act g (a,b,c) = cpsAct g a b c (,,)

act' ::  S3 -> Triple t -> Triple t
act' g (Triple a b c) = cpsAct g a b c Triple

-- {-# INLINE act #-}
--act ::  S3 -> Triple t -> Triple t
-- act g = case g of
--                        S3abc -> \(Triple a b c) -> Triple a b c
--                        S3bca -> \(Triple a b c) -> Triple b c a
--                        S3cab -> \(Triple a b c) -> Triple c a b
--                        S3acb -> \(Triple a b c) -> Triple a c b
--                        S3bac -> \(Triple a b c) -> Triple b a c
--                        S3cba -> \(Triple a b c) -> Triple c b a


data ABC = A | B | C deriving(Eq)
abc = Triple A B C

toFun ::  S3 -> ABC -> ABC
toFun g = case g of
                       S3abc -> mkFun A B C
                       S3bca -> mkFun B C A
                       S3cab -> mkFun C A B
                       S3acb -> mkFun A C B
                       S3bac -> mkFun B A C
                       S3cba -> mkFun C B A
    where
        mkFun imA _ _ A = imA
        mkFun _ imB _ B = imB
        mkFun _ _ imC _ = imC


fromFun ::  (ABC -> ABC) -> S3
fromFun f = s3the (\g ->  
                    let toFun_g = toFun g 
                    in
                          toFun_g A == f_A 
                       && toFun_g B == f_B)
    where
        f_A = f A
        f_B = f B



unAct f = s3the (\g ->  act' g abc === y)
    where 
        y = f abc
        Triple a b c === Triple a' b' c' = a==a' && b==b' 




s3mult ::  S3 -> S3 -> S3
s3mult g2 g1 = fromFun (toFun g2 . toFun g1)


s3inv g = s3the (\g' -> s3mult g' g == S3abc)

s3the ::  (S3 -> Bool) -> S3
s3the p 
    | p S3abc = S3abc
    | p S3acb = S3acb
    | p S3bac = S3bac
    | p S3bca = S3bca
    | p S3cba = S3cba
    | p S3cab = S3cab
    | otherwise = error "s3the_error"



instance Monoid S3 where
    mempty = S3abc
    mappend = s3mult

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

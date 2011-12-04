import TypeLevel.NaturalNumber
import TypeLevel.NaturalNumber.Operations
import Data.Category
import Data.Ord
import Data.NaturalNumber

-- data FaceMap i k = FaceMap 
-- data DegeneracyMap i k = DegeneracyMap



data Ordering_N n where
    -- | Contains the predecessor of the second argument to 'compareN'
    LT_N :: N n -> Ordering_N (SuccessorTo n)
    EQ_N :: Ordering_N n
    -- | Contains the predecessor of the first argument to 'compareN'
    GT_N :: N n -> Ordering_N (SuccessorTo n)

compareN :: N n -> N n -> Ordering_N n
compareN NZero NZero = EQ_N
compareN NZero (NSuccessorTo j) = LT_N j
compareN (NSuccessorTo i) NZero = GT_N i
compareN (NSuccessorTo i) (NSuccessorTo j) = 
    case compareN i j of
         LT_N _ -> LT_N i
         x@EQ_N -> x
         GT_N _ -> GT_N j


data SimplexCat n n' = SimplexCat [Int] [Int] 


-- data SimplexCatGen n n' where
--     FaceMap :: N n -> SimplexCatGen n (SuccessorTo n) 
--     DegeneracyMap :: N (SuccessorTo n) -> SimplexCatGen (SuccessorTo n) n
-- 
-- data SimplexCat n n' where
--     Id :: SimplexCat n n
--     C :: SimplexCatGen n' n'' -> SimplexCat n n' -> SimplexCat n n''



-- normalize :: SimplexCat n n' -> SimplexCat n n'
-- 
-- normalize (C f1 f2rest@(C f2 rest)) = case (f1,f2) of
-- 
--     (FaceMap j, FaceMap i) ->
--         case compareN i j of
--              LT_N pred_j -> 
--                 normalize (C f2 (normalize (C (FaceMap pred_j) rest)))
-- 
--              _ -> 
--                 normalize (C f1 (normalize f2rest))
-- 
--     | DegeneracyMap j <- f1, DegeneracyMap i <- f2 =
-- 
--         case compareN j i of
--     
--         normalize (C f2 (normalize (C (FaceMap (SuccessorTo j) rest))))
-- 
--     | 
-- 

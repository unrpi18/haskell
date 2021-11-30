module Sort where

insert x [] = [x]
insert x (y : ys)
 | x < y = x:y:ys
 | otherwise = y: insert x ys

insertSort [] = []
insertSort (x : xs) = insert x (insertSort xs)

{-
#[3,4,5] [1,2,3]
# 3> 1 -> [1] :merge [ 3 4 5] [2 3]
# 3 > 2 -> [1] : [2] :merge [3,4,5] [3]
# 3 <=3 -> [1] :[2] :[3] :merge [4,5] [3]
# 4> 3 -> [1] :[2] :[3] :[3] : merge [4,5][]
# [1]:[2]:[3]:[3]:[4,5]
-}
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y:ys)
 | x <= y  = x : merge xs (y:ys)
 | otherwise = y : merge (x:xs) ys

{-[3,4,6,2,1,5,9,8]
merge (3 4 6 2) merge (1 5 9 8)
merge (3 4) (merge 6 2) merge(1 5) merge (9 8)
..
-}
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge
 (mergeSort (take (length xs  `div` 2) xs))
 (mergeSort (drop (length xs `div` 2) xs))

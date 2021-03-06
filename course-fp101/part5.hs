merge :: Ord a => [a] -> [a] -> [a]

merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) =
	if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

ex9 = merge [2,5,6] [1,3,4]


-- ex10
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
	where (ys, zs) = halve xs
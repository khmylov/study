all' :: (a -> Bool) -> [a] -> Bool
all' p xs = foldl (&&) True (map p xs)

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = foldr (\x acc -> (p x) || acc) False xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
--takeWhile' _ [] = []
--takeWhile' p (x:xs)
--	| p x = x : takeWhile' p xs
--	| otherwise = []
takeWhile' p = foldl (\ acc x -> if p x then x : acc else acc) []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
	| p x = dropWhile' p xs
	| otherwise = xs

--map' :: (a -> b) -> [a] -> [b]
--map' f = foldl (\xs x -> xs ++ [f x]) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

dec2int :: [Integer] -> Integer
dec2int = foldl (\x y -> x * 10 + y) 0

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \ x y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x 
	| p x = []
	| otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f
import Data.Char

pyths n = 
	[(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n],
	 x ^ 2 + y ^ 2 == z ^ 2]

factors x = 
	[n | n <- [1..x], x `mod` n == 0]

perfects n = 
	[x | x <- [1..n], isPerfect x]
	where isPerfect num = sum (init (factors num)) == num

ex4' = [(x, y) | x <- [1,2,3], y <- [4,5,6]]
ex4 = concat [[(x, y) | y <- [4,5,6]] | x <- [1..3]]

-- ex5
find k t = [v | (k', v) <- t, k == k']
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
	where n = length xs - 1

-- ex6
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
ex6 = scalarproduct [1..3] [4..6]

-- ex7
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c 
	| isLower c = int2let ((let2int c + n) `mod` 26)
	| isUpper c = toUpper (shift n (toLower c))
	| otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

ex7 = encode 13 "Think like a Fundamentalist Code like a Hacker"


-- ex8
ex8 = [(x, y) | x <- [1,2], y <- [1,2]]

-- ex9
ex9 = [x | x <- [1,2,3], y <- [1..x]]

-- ex10
ex10 = sum [x | x <- [1..10], even x]

-- ex12
riffle xs ys = concat [[x, y] | (x, y) <- zip xs ys]
ex12 = riffle [1,2,3] [4,5,6]

-- ex13
divides x y = x `mod` y == 0
divisors x = [d | d <- [1..x], x `divides` d]
ex13 = (divisors 15, divides 15 2, divides 15 3)
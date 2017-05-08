{-# LANGUAGE NPlusKPatterns #-}

import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

-- ex0
natToInteger :: Nat -> Integer

natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

--natToInteger (Succ n) = natToInteger n + 1
--natToInteger Zero = 0

--natToInteger (Succ n) = 1 + natToInteger n
--natToInteger Zero = 0

--natToInteger = head . m
--    where m Zero = [0]
--          m (Succ n) = [sum [x | x <- (1 : m n)]]

--natToInteger = \n -> genericLength [c | c <- show n, c == 'S']

ex1 = natToInteger $ Succ $ Succ Zero


-- ex1

integerToNat :: Integer -> Nat

integerToNat (n+1) = Succ (integerToNat n)
integerToNat 0 = Zero

--integerToNat 0 = Zero
--integerToNat (n+1) = Succ (integerToNat n)

--integerToNat (n+1) = let m = integerToNat n in Succ m
--integerToNat 0 = Zero


-- ex2

add :: Nat -> Nat -> Nat

--add Zero n = n
--add (Succ m) n = Succ (add n m)

--add (Succ m) n = Succ (add n m)
--add Zero n = n

--add n Zero = n
--add n (Succ m) = Succ (add m n)

add n (Succ m) = Succ (add m n)
add n Zero = n

ex2 = natToInteger $ add (integerToNat 42) (integerToNat 18)


-- ex3

mult :: Nat -> Nat -> Nat

mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

ex3 = natToInteger (mult (integerToNat 7) (integerToNat 8))


-- ex4

--data Tree = Leaf Integer
--          | Node Tree Integer Tree

--occurs :: Integer -> Tree -> Bool

----occurs m (Leaf n) = m == n
----occurs m (Node l n r) = 
----    case compare m n of 
----        LT -> occurs m l
----        EQ -> True
----        GT -> occurs m r

--occurs m (Leaf n) = m == n
--occurs m (Node l n r) 
--    | m == n = True
--    | m < n = occurs m l
--    | otherwise = occurs m r

--ex4tree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 6) 8 (Leaf 9))


-- ex5
data Tree = Leaf Integer
          | Node Tree Tree
          deriving Show

balanced :: Tree -> Bool

leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r
balanced (Leaf _) = True
balanced (Node l r) = 
    abs (leaves l - leaves r) <= 1 && balanced l && balanced r


-- ex6
balance :: [Integer] -> Tree

halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
    where (ys, zs) = halve xs


-- ex11
class Monoid a where
    mempty :: a
    (<>) :: a -> a -> a

instance Monoid [a] where
    mempty = []
    (<>) = (++)


-- ex13
class (Functor f) => Foldable f where
    fold :: (Monoid m) => f m -> m

instance Foldable [] where
    fold = foldr (<>) mempty
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs


-- ex2
putStrLn' :: String -> IO ()

--putStrLn' [] = putChar '\n'
--putStrLn' xs = putStr' xs >> putStrLn' ""

--putStrLn' [] = putChar '\n'
--putStrLn' xs = putStr' xs >> putChar '\n'

--putStrLn' [] = putChar '\n'
--putStrLn' xs = putStr' xs >>= \x -> putChar '\n'

putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStr' "\n"


-- ex3
getLine' :: IO String

getLine' = get ""
get :: String -> IO String
get xs 
  = do x <- getChar
       case x of 
           '\n' -> return xs
           _ -> get (xs ++ [x])


-- ex4
interact' :: (String -> String) -> IO ()
interact' f = 
    do input <- getLine'
       putStrLn' (f input)


-- ex5
sequence_' :: Monad m => [m a] -> m ()

--sequence_' [] = return ()
--sequence_' (m : ms) = (foldl (>>) m ms) >> return ()

--sequence_' [] = return ()
--sequence_' (m : ms) = m >> sequence_' ms

--sequence_' [] = return ()
--sequence_' (m:ms) = m >>= \_ -> sequence_' ms

sequence_' ms = foldr (>>) (return ()) ms


-- ex6
sequence' :: Monad m => [m a] -> m [a]

--sequence' [] = return []
--sequence' (m : ms) =
--    m >>=
--        \a ->
--        do as <- sequence' ms
--           return (a : as)

--sequence' ms = foldr func (return []) ms
--    where
--        func :: (Monad m) => m a -> m [a] -> m [a]
--        func m acc = 
--            do x <- m
--               xs <- acc
--               return (x : xs)

sequence' [] = return []
sequence' (m : ms) = 
    do a <- m
       as <- sequence' ms
       return (a : as)


-- ex7
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]

--mapM' f as = sequence' (map f as)

--mapM' f [] = return []
--mapM' f (a : as) =
--    f a >>= \b -> mapM' f as >>= \bs -> return (b : bs)

--mapM' f [] = return []
--mapM' f (a : as) = 
--    do b <- f a
--       bs <- mapM' f as
--       return (b : bs)

mapM' f [] = return []
mapM' f (a:as) = 
    f a >>=
        \b ->
        do bs <- mapM' f as
           return (b : bs)

ex7 = mapM' (\x -> Just $ x * 2) [1..10]


-- ex8
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x : xs) = 
    do flag <- p x
       ys <- filterM' p xs
       if flag then return (x : ys) else return ys


-- ex9
foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a

foldLeftM _ a [] = return a
foldLeftM f a (x : xs) = 
    f a x >>= \b -> foldLeftM f b xs

ex9 = foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r


-- ex10
foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b [] = return b
foldRightM f b (a:as) = 
    foldRightM f b as >>= \b' -> f a b'

ex10 = foldRightM (\a b -> putChar a >> return (a:b)) [] (show [1,3..10]) >>= \r -> putStrLn r


-- ex11
liftM :: Monad m => (a -> b) -> m a -> m b 

--liftM f m =
--    do x <- m
--       return (f x)

liftM f m = m >>= \a -> return (f a)

ex11a = liftM (+2) $ Just 3
ex11b = liftM (\a -> [a, a]) getChar
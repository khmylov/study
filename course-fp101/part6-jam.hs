--type Church a = (a -> a) -> a -> a

--zero :: Church a
zero = \s z -> z
one = \s z -> s z
--two = \s -> s (s z)
--two = \s z -> (s . s)  z
two = \s -> (s . s)

c2i x = x (+1) 0
c2s x = x ('*':) ""

add x y = \s z -> x s (y s z)
--mul x y = \s z -> x (y s) z
mul x y = \s z => x . y
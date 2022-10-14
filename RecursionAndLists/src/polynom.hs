-- Вычисление многочлена за O(n)
-- Елизавета Мойсейчик

evalHelper :: (Eq t, Num t, Num a, Enum t) => [a] -> a -> t -> a
evalHelper c x 1 = head c
evalHelper c x n = x * evalHelper (tail c) x (pred n) + head c

evalPolynomial :: Num a => [a] -> a -> a
evalPolynomial c x = evalHelper (reverse c) x (length c)
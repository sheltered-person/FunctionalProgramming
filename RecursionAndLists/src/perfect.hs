-- Совершенные числа
-- Елизавета Мойсейчик

isPerfect :: Int -> Bool
isPerfect n = n == sum [i | i <- [1..n - 1],  mod n i == 0]
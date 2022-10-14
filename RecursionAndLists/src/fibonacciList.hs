-- Список чисел Фибоначчи
-- Елизавета Мойсейчик

generalizedFibonacci :: [Int] -> [Int]
generalizedFibonacci [] = []
generalizedFibonacci (a : as) = a : generalizedFibonacci (as ++ [sum (a : as)])

nGenFibonacci :: Int -> [Int] -> [Int]
nGenFibonacci n as = take n $ generalizedFibonacci as
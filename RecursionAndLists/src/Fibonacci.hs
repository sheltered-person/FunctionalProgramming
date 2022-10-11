-- Фибоначчи возведением матриц в степень за O(log n)
-- Елизавета Мойсейчик

import Data.List (transpose)

-- Перемножение матриц
mult :: Num a => [[a]] -> [[a]] -> [[a]]
mult a b = [[ sum $ zipWith (*) aRow bCol | bCol <- transpose b ] | aRow <- a ]

-- Реализации обычного и улучшенного возведения в степень
power :: (Eq n, Integral n, Enum n, Num a) => [[a]] -> n -> [[a]]
power a n
    | n == 0 = [[1, 1], [1, 1]]
    | n == 1 = a
    | otherwise = mult a $ power a $ pred n

advancedPower :: (Eq n, Integral n, Enum n, Num a) => [[a]] -> n -> [[a]]
advancedPower a n
    | n == 0 = [[1, 1], [1, 1]]
    | n == 1 = a
    | even n = advancedPower (mult a a) (div n 2)
    | otherwise = mult a $ power a $ pred n

-- Наивная реализация Фибоначчи для тестирования
naiveFibonacci :: Int -> Int
naiveFibonacci n = head $ last $ power [[0, 1], [1, 1]] n

-- Ускоренная реализация Фибоначчи
advancedFibonacci :: Int -> Int
advancedFibonacci n = head $ last $ power [[0, 1], [1, 1]] n
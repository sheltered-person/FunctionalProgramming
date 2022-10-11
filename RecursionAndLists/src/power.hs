-- Возведение в степень за O(log n)
-- Елизавета Мойсейчик

power :: Int -> Int -> Int
power n x
    | n == 0 = 1
    | n == 1 = x
    | even n = power (div n 2) (x * x)
    | otherwise = x * power (n - 1) x
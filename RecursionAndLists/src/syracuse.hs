import System.Win32 (COORD(x))
-- Сиракузская последовательность
-- Елизавета Мойсейчик

nextElement :: Integral a => a -> a
nextElement n
    | even n = div n 2
    | otherwise = 3 * n + 1


greaterThenZero :: (Ord a, Num a) => a -> Bool
greaterThenZero x = x > 0

syracuseSequence :: Int -> [Int] -> [Int]
syracuseSequence n xs
    | n == 1 = xs ++ [1]
    | otherwise = syracuseSequence (nextElement n) $ xs ++ [n]

collatz :: Int -> Int
collatz x = length $ syracuseSequence x []
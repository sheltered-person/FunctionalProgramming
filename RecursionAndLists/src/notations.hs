-- Системы счисления
-- Елизавета Мойсейчик

fromDigitsHelper :: [Int] -> Int -> Int -> Int
fromDigitsHelper [] _ _ = 0
fromDigitsHelper (a:as) n i = a + n * fromDigitsHelper as n (succ i)

fromDigits :: Int -> [Int] -> Int
fromDigits n a = fromDigitsHelper a n 0

toDigitsHelper :: Int -> Int -> [Int]
toDigitsHelper n 0 = []
toDigitsHelper n a =
    let (d, m) = divMod a n
    in m : toDigitsHelper n d

toDigits :: Int -> Int -> [Int]
toDigits n a = reverse (toDigitsHelper n a)
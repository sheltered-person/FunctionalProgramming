import Data.List (unfoldr, nub)
-- Елизавета Мойсейчик

-- Развернуть натуральное число n в список всех чисел, меньших его.
lessUnfoldPair :: (Ord b, Num b) => b -> b -> Maybe (b, b)
lessUnfoldPair n x
    | n > x = Just (x, x + 1)
    | otherwise = Nothing

unfoldToLess :: (Ord b, Num b) => b -> [b]
unfoldToLess n = unfoldr (lessUnfoldPair n) 1


-- Развернуть число в список разрядов его двоичного представления

-- P.S. Не смогла додуматься, как не применяя reverse к результату 
-- получить привычный порядок возрастания веса разрядов справа налево, 
-- но наверняка можно это сделать более элегантным способом.
binUnfoldPair :: Integral b => b -> Maybe (b, b)
binUnfoldPair x
    | x == 0 = Nothing
    | otherwise = Just (mod x 2, div x 2)

unfoldToBin :: Integral b => b -> [b]
unfoldToBin = reverse . unfoldr binUnfoldPair


-- Список разрядов числа преобразовать свёрткой в значение числа.
binFoldPair :: (Eq a1, Num a1, Num a2) => (a2, a2) -> a1 -> (a2, a2)
binFoldPair (a, b) c
    | c == 1 = (a + b, b * 2)
    | otherwise = (a, b * 2)

foldToDecimal :: (Eq a1, Num a1, Num b) => [a1] -> b
foldToDecimal d = fst $ foldl binFoldPair (0, 1) $ reverse d


-- Развернуть число в список его простых делителей
factor :: Integral t => t -> t -> Maybe (t, t)
factor k n
    | n < 2 = Nothing
    | mod n k == 0 = Just (k, div n k)
    | otherwise = factor (k + 1) n

unfoldToFactors :: Integer -> [Integer]
unfoldToFactors = nub . unfoldr (factor 2)


-- Выразить список первых n чисел Фибоначчи через развёртку
fibUnfoldPair :: Num b => (b, b) -> Maybe (b, (b, b))
fibUnfoldPair (a, b) = Just (a, (b, a + b))

unfoldToFib :: Num a => Int -> [a]
unfoldToFib n = take n $ unfoldr fibUnfoldPair (0, 1)


-- Развернуть число в сиракузскую последовательность
syrUnfoldPair :: Integral a => a -> Maybe (a, a)
syrUnfoldPair a
    | a == 0 = Nothing
    | a == 1 = Just (1, 0)
    | even a = Just (a, div a 2)
    | otherwise = Just (a, 3 * a + 1)

unfoldToSyr :: Integral b => b -> [b]
unfoldToSyr = unfoldr syrUnfoldPair

-- Выразить список простых чисел, не превышающих n, через развёртку
isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. (floor . sqrt . fromInteger) n], mod n x == 0]

primeUnfoldPair :: Integer -> Maybe (Integer, Integer)
primeUnfoldPair n
    | isPrime n = Just (n, succ n)
    | otherwise = primeUnfoldPair $ succ n

unfoldToPrimes :: Integer -> [Integer]
unfoldToPrimes n = takeWhile (< n) $ unfoldr primeUnfoldPair 1
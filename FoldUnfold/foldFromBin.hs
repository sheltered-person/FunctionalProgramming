import Data.List (unfoldr, nub)
-- Елизавета Мойсейчик

-- Список разрядов числа преобразовать свёрткой в значение числа.
binFoldPair :: (Eq a1, Num a1, Num a2) => (a2, a2) -> a1 -> (a2, a2)
binFoldPair (a, b) c
    | c == 1 = (a + b, b * 2)
    | otherwise = (a, b * 2)

foldToDecimal :: (Eq a1, Num a1, Num b) => [a1] -> b
foldToDecimal d = fst $ foldl binFoldPair (0, 1) $ reverse d
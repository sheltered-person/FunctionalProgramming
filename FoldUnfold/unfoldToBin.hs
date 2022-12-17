import Data.List (unfoldr, nub)
-- Елизавета Мойсейчик

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
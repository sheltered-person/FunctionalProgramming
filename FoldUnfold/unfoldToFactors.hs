import Data.List (unfoldr, nub)
-- Елизавета Мойсейчик

-- Развернуть число в список его простых делителей
factor :: Integral t => t -> t -> Maybe (t, t)
factor k n
    | n < 2 = Nothing
    | mod n k == 0 = Just (k, div n k)
    | otherwise = factor (k + 1) n

unfoldToFactors :: Integer -> [Integer]
unfoldToFactors = nub . unfoldr (factor 2)
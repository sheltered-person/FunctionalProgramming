import Data.List (unfoldr, nub)
-- Елизавета Мойсейчик

-- Развернуть число в сиракузскую последовательность
syrUnfoldPair :: Integral a => a -> Maybe (a, a)
syrUnfoldPair a
    | a == 0 = Nothing
    | a == 1 = Just (1, 0)
    | even a = Just (a, div a 2)
    | otherwise = Just (a, 3 * a + 1)

unfoldToSyr :: Integral b => b -> [b]
unfoldToSyr = unfoldr syrUnfoldPair
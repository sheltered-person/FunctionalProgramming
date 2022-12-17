import Data.List (unfoldr, nub)
-- Елизавета Мойсейчик

-- Развернуть натуральное число n в список всех чисел, меньших его.
lessUnfoldPair :: (Ord b, Num b) => b -> b -> Maybe (b, b)
lessUnfoldPair n x
    | n > x = Just (x, x + 1)
    | otherwise = Nothing

unfoldToLess :: (Ord b, Num b) => b -> [b]
unfoldToLess n = unfoldr (lessUnfoldPair n) 1
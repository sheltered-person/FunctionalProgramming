import Data.List (unfoldr, nub)
-- Елизавета Мойсейчик

-- Выразить список первых n чисел Фибоначчи через развёртку
fibUnfoldPair :: Num b => (b, b) -> Maybe (b, (b, b))
fibUnfoldPair (a, b) = Just (a, (b, a + b))

unfoldToFib :: Num a => Int -> [a]
unfoldToFib n = take n $ unfoldr fibUnfoldPair (0, 1)
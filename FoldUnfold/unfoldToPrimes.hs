import Data.List (unfoldr, nub)
-- Елизавета Мойсейчик

-- Выразить список простых чисел, не превышающих n, через развёртку
isPrime :: Integer -> Bool
isPrime n = (n > 1) && null [x | x <- [2 .. (floor . sqrt . fromInteger) n], mod n x == 0]

primeUnfoldPair :: Integer -> Maybe (Integer, Integer)
primeUnfoldPair n
    | isPrime n = Just (n, succ n)
    | otherwise = primeUnfoldPair $ succ n

unfoldToPrimes :: Integer -> [Integer]
unfoldToPrimes n = takeWhile (< n) $ unfoldr primeUnfoldPair 1
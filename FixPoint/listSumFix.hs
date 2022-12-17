-- Елизавета Мойсейчик
-- Комбинатор неподвижной точки

-- Сумма списка

import Data.Function

sumOfListHelper :: ([Int] -> Int -> Int) -> [Int] -> Int -> Int
sumOfListHelper f [] sum = sum
sumOfListHelper f (currentElem : tail) sum = f tail (sum + currentElem)

sumOfList :: [Int] -> Int
sumOfList list = fix sumOfListHelper list 0
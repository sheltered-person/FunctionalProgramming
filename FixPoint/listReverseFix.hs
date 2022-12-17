-- Елизавета Мойсейчик
-- Комбинатор неподвижной точки

-- Переворачивание списка

import Data.Function

revertListHelper :: ([Int] -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int]
revertListHelper f [] reverted = reverted
revertListHelper f (currentElem : tail) reverted = f tail (currentElem : reverted)

revertList :: [Int] -> [Int]
revertList list = fix revertListHelper list []
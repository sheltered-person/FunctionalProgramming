-- Елизавета Мойсейчик
-- Комбинатор неподвижной точки

-- Минимальный элемент в списке

import Data.Function

minListElemHelper :: ([Int] -> Int -> Int) -> [Int] -> Int -> Int
minListElemHelper f [] minElem = minElem
minListElemHelper f (currElem : tail) tempMin = 
    if currElem < tempMin 
        then f tail currElem 
    else f tail tempMin

minListElem :: [Int] -> Int
minListElem (currElem : tail) = fix minListElemHelper tail currElem
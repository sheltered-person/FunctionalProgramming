-- Елизавета Мойсейчик
-- Комбинатор неподвижной точки

-- Наибольший общий делитель

import Data.Function

gcdFuncHelper:: (Int -> Int -> Int) -> Int -> Int -> Int
gcdFuncHelper f a b
  | a > b = f (a - b) b
  | a < b = f a (b - a)
  | otherwise = a

gcdFunc:: Int -> Int -> Int
gcdFunc = fix gcdFuncHelper
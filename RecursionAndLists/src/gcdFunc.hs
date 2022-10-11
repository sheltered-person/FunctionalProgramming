
-- Наибольший общий делитель
-- Елизавета Мойсейчик

gcdFunc :: Int -> Int -> Int
gcdFunc a b
  | a > b = gcdFunc (a - b) b
  | a < b = gcdFunc a (b - a)
  | otherwise = a
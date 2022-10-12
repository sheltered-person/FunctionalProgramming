-- Числа Делануа
-- Елизавета Мойсейчик

-- Реализация "Где-то расстроилась Елена Павловна Соболевская"
delannoy :: Int -> Int -> Int
delannoy m n 
    | m == 0 || n == 0 = 1
    | otherwise = delannoy (m - 1) n + delannoy m (n - 1) + delannoy (m - 1) (n - 1)
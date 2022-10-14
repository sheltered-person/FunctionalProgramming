-- Сшивание списков бинарной операцией
-- Елизавета Мойсейчик

xZipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
xZipWith _ [] _ = []
xZipWith _ _ [] = []
xZipWith f (x:xl) (y:yl) = f x y : xZipWith f xl yl
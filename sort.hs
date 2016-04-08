qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = small ++ [x] ++ big
    where
        small = qsort [a | a <- xs, a <= x]
        big = qsort [a | a <- xs, a > x]

fib :: (Num a, Ord a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

itemAt :: (Num i, Ord i) => i -> [a] -> a
itemAt _ [] = error "index out of bounds"
itemAt 0 (x:xs) = x
itemAt i (x:xs) = itemAt (i-1) xs

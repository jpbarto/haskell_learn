import Text.Printf

innerLoop :: Int -> Int -> Int -> Int
innerLoop i n sum = foldl (\acc j -> acc + 1) sum [(i+1)..n]

outerLoop :: Int -> Int -> Int
outerLoop n s = foldl (\acc i -> acc + (innerLoop i n s)) s [1..(n*n*n)]

nestedFors :: Int -> Int
nestedFors n = sum [1 | i <- [1..(n*n*n)], j <- [i..n]]

calculate :: Int -> IO ()
calculate n = printf "for n = %d the sum is %d\n" n $ nestedFors n

main = do
    printf "for n = 16 the sum is %d\n" $ outerLoop 16 0
    sequence [ calculate $ 2^x | x <- [4..10]]

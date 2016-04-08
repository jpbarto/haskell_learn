fx f g h = do
    a <- f
    b <- g
    c <- h
    return (a, b, c)

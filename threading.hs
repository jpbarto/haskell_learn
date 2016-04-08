import Control.Concurrent
import Control.Monad (replicateM_)

data RollingAverage = Average {
        average :: Double,
        valueCount :: Integer
    } deriving (Show)

type AverageMVar = MVar RollingAverage

calc_average :: RollingAverage -> Double -> RollingAverage
calc_average ra n = Average {
    average = ((average ra)*(fromInteger (valueCount ra)) + n)/(fromInteger (valueCount ra) + 1),
    valueCount = ((valueCount ra) + 1)
}

-- list_average :: Real n => [n] -> RollingAverage
list_average :: [Double] -> RollingAverage
list_average [] = Average 0 0
list_average (x:[]) = Average (_to_double x) 1
-- list_average (x:xs) = calc_average (list_average xs) (_to_double x)
list_average xs = foldl (\avg num -> Average (((average avg)*(fromInteger (valueCount avg)) + num)/(fromInteger (valueCount avg) + 1)) ((valueCount avg) + 1)) (Average 0 0) xs

_to_double :: Real n => n -> Double
_to_double = fromRational . toRational

compute_average :: Real n => MVar RollingAverage -> [n] -> IO ThreadId
compute_average ret xs = forkIO $ (putMVar ret (list_average xs))

main = do
        mvar <- newEmptyMVar
        mapM_ (compute_average mvar) ranges
        replicateM_ (length ranges) (takeMVar mvar >>= (putStrLn . show))
    where
        ranges = [[1..500], [1..5000], [1..1000000], [x*1.5 | x <- [1.1000000]], [x*2 | x <- [1..1000000]], [x^2 | x <- [1..1000000]]]

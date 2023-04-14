import Data.List
import System.IO
import Data.Map qualified as Map

-- primeFactors :: (Integral a) => a -> Map.Map a a


primeFactorsList :: (Integral a) => a -> [a]
primeFactorsList n = do
    let p = find (\x -> n `mod` x == 0 && (fromIntegral x <= (sqrt $ fromIntegral n :: Float))) [2,3..n]
    case p of
        Nothing -> [n]
        Just x -> x : primeFactorsList (n `div` x)


primeFactorsMap :: (Integral a) => a -> [(a, a)]
primeFactorsMap n = map (\x -> (head x, fromIntegral $ length x)) $ group $ primeFactorsList n


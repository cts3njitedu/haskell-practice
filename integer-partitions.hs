import Data.List
import System.IO ()
import Data.Either

data IntegerPartitionError = NEGATIVE_NUMBER | NUMBER_TOO_BIG deriving (Show, Eq)

getIntegerPartitionsWithPart :: (Integral a) => a -> a -> [[a]]
getIntegerPartitionsWithPart x y
    | x < 0 || y < 0 = [[]]
    | x > 50 || y > 50 = [[]]
getIntegerPartitionsWithPart 0 y = [[y]]
getIntegerPartitionsWithPart x 0 = concat [getIntegerPartitionsWithPart n (x-n) | n <- map (x-) [x, x-1..1]]
getIntegerPartitionsWithPart x y = map (y:) $ concat [getIntegerPartitionsWithPart n (x-n) | let z = min x y, n <- map (x-) [z, z-1..1]]

getIntegerPartitions :: (Integral a) => a -> [[a]]
getIntegerPartitions x = getIntegerPartitionsWithPart x 0

getIntegerPartitions2 :: (Integral a, Show a) => a -> Either String [[a]]
getIntegerPartitions2 x = getIntegerPartitionsWithPart2 x 0

getIntegerPartitionsWithPart2 :: (Integral a, Show a) => a -> a -> Either String [[a]]
getIntegerPartitionsWithPart2 x y
    | x < 0 || y < 0 = Left ("Error " ++ show NEGATIVE_NUMBER ++ ". Both the sum and partition have to be non-negative." 
        ++ "(sum, partition): (" ++ show x ++ ", " ++ show y ++ ")")
    | x > 50 || y > 50 = Left ("Error " ++ show NUMBER_TOO_BIG ++ ". Both the sum and partition have to be less than or equal to 50." 
        ++ "(sum, partition): (" ++ show x ++ ", " ++ show y ++ ")")
getIntegerPartitionsWithPart2 0 y = Right [[y]]
getIntegerPartitionsWithPart2 x y = do
    let subParts = [getIntegerPartitionsWithPart2 n (x-n) | let z = if y==0 then x else min x y, n <- map (x-) [z, z-1..1]]
    let partitionError = find isLeft subParts
    case partitionError of
        Nothing -> if y == 0
            then Right (concat $ rights subParts)
            else Right (map (y:) $ concat $ rights subParts)
        Just x -> x

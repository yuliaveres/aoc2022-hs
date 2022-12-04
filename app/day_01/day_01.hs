import Data.List.Split
import Data.List
import Data.Maybe

part1 :: String -> Int
part1 input = do 
  maximum (groupValues input)

part2 :: String -> Int
part2 input = do
  let values = reverse (sort (groupValues input))
  sum (take 3 values)

groupValues :: String -> [Int]
groupValues input = do
  let groups = splitOn "\n\n" input
  [ groupValue g | g <- groups ]
  
groupValue :: String -> Int
groupValue group = do
  let groupItems = words group
  let values = [ read val :: Int | val <- groupItems ]
  sum values
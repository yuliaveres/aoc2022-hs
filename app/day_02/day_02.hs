part1 :: String -> Int
part1 input = do
   let rounds = lines input
   let scores = [ roundScore (head r) (last r) + (shapeScore (last r)) | r <- rounds ]
   sum scores

part2 :: String -> Int
part2 input = do
   let rounds = lines input
   let scores = [ shapeScore (expectedShape (head r) (last r)) + (outcomeScore (last r)) | r <- rounds ]
   sum scores

roundScore :: Char -> Char -> Int
roundScore 'A' 'Y' = 6
roundScore 'B' 'Z' = 6
roundScore 'C' 'X' = 6
roundScore 'A' 'Z' = 0
roundScore 'B' 'X' = 0
roundScore 'C' 'Y' = 0
roundScore _ _ = 3

shapeScore :: Char -> Int
shapeScore 'A' = 1
shapeScore 'X' = 1
shapeScore 'B' = 2
shapeScore 'Y' = 2
shapeScore 'C' = 3
shapeScore 'Z' = 3

outcomeScore :: Char -> Int
outcomeScore 'X' = 0
outcomeScore 'Y' = 3
outcomeScore 'Z' = 6

expectedShape :: Char -> Char -> Char
expectedShape 'A' 'X' = 'C'
expectedShape 'C' 'Y' = 'C'
expectedShape 'B' 'Z' = 'C'
expectedShape 'A' 'Y' = 'A'
expectedShape 'B' 'X' = 'A'
expectedShape 'C' 'Z' = 'A'
expectedShape 'A' 'Z' = 'B'
expectedShape 'B' 'Y' = 'B'
expectedShape 'C' 'X' = 'B'
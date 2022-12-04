import Data.List
import Data.Maybe
import Data.List.Split



part1 :: String -> Int 
part1 input = do
    let rucksacks = lines input
    let _ = print rucksacks
    let letters = ['a'..'z'] ++ ['A'..'Z']
    let scores = [1..26] ++ [27..52]
    let zipped = zip letters scores
    let result = [ repetitive c | c <- rucksacks]
    let s = [ score zipped l | l <- result ]
    sum s

score :: [(Char, Int)] -> Char -> Int
score zipped letter = snd (fromJust (find (\c -> fst c == letter) zipped))
    
repetitive :: [Char] -> Char
repetitive line = do
    let count = length line
    let parts = splitAt (div count 2) line
    let first = fst parts
    let second = snd parts
    let intersection = commonItems first second
    (head intersection)

commonItems first second = [ c | c <- first, elem c second]

part2 :: String -> Int 
part2 input = do
    let rucksacks = lines input
    let chunks = chunksOf 3 rucksacks
    let inter = [ head (intersect (ch !! 0) ( intersect (ch !! 1) (ch !! 2)) ) | ch <- chunks ]

    let letters = ['a'..'z'] ++ ['A'..'Z']
    let scores = [1..26] ++ [27..52]
    let zipped = zip letters scores
    let s = [score zipped l | l <- inter ]
    sum s
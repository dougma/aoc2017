module Day13 where

import Data.Array
import Data.Int(fromString)
import Data.Maybe
import Data.String(split, trim, Pattern(..))
import Data.String.Utils as StringUtils  
import Data.Tuple
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Prelude

type Scanner = Array (Tuple Int Int)

exampleInput = """0: 3
1: 2
4: 4
6: 4"""

goExample = part2 $ parse exampleInput

go = do
    input <- readTextFile UTF8 "src/Day13.txt"
    pure $ part2 $ parse input

part1 input = foldl (+) 0 $ map (severityWithDelay 0) input
part2 input = findDelay 0 input

findDelay :: Int -> Scanner -> Int
findDelay delay input = 
    if any (isHit delay) input then
        findDelay (delay + 1) input
    else
        delay

isHit delay (Tuple depth range) = 
    (delay + depth) `mod` (period range) == 0

severityWithDelay delay layer =
    if isHit delay layer then
        fst layer * snd layer
    else
        0

period 1 = 1
period x = (x - 1) + (x - 1)

parse input = foldl parseLine [] $ StringUtils.lines input

parseLine :: Scanner -> String -> Scanner
parseLine scanner line =
    case split (Pattern ":") line of
        [layer, range] -> 
            case fromString layer, fromString $ trim range of
                Just nLayer, Just nRange -> snoc scanner (Tuple nLayer nRange)
                _, _ -> scanner
        _ -> scanner

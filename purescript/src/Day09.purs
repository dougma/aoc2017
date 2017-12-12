module Day09 where

import Prelude
import Data.Array(foldl)
import Data.String(toCharArray)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data ParseState = Group | Garbage | Cancel
type State = { score :: Int, garbage:: Int, level :: Int, state :: ParseState }

start :: State
start = { score: 0, garbage: 0, level: 0, state: Group }

go = do
    input <- readTextFile UTF8 "src/Day09.txt"
    pure $ goString input

goString s = [result.score, result.garbage]
    where result = foldl step start $ toCharArray s

nextState :: ParseState -> Char -> ParseState
nextState Group '<' = Garbage
nextState Group _ = Group
nextState Garbage '>' = Group
nextState Garbage '!' = Cancel
nextState Garbage _ = Garbage
nextState Cancel _ = Garbage

levelChange :: ParseState -> Char -> Int
levelChange Group '{' = 1
levelChange Group '}' = -1
levelChange _ _ = 0

scoreChange :: ParseState -> Char -> Int -> Int
scoreChange Group '{' level = level + 1
scoreChange _ _ _ = 0

countGarbage :: ParseState -> ParseState -> Int
countGarbage Garbage Garbage = 1
countGarbage _ _ = 0

step :: State -> Char -> State
step { score: score, garbage: garbage, level: level, state: state } ch =
    {
        score: score + scoreChange state ch level,
        garbage: garbage + countGarbage state next,
        level: level + levelChange state ch,
        state: next
    }
    where next = nextState state ch

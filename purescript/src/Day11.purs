module Day11 where

import Prelude

import Data.Array (foldl)
import Data.String (split, Pattern(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

go = do
    input <- readTextFile UTF8 "src/Day11.txt"
    pure $ result input

type Xy = { x :: Int, y :: Int }
type State = { pos :: Xy, dist :: Int }

follow :: Xy -> String -> Xy
follow {x, y} dir = 
    case dir of
        "n" -> {x: x, y: y-1}
        "s" -> {x: x, y: y+1}
        "nw" -> {x: x-1, y: y-1}
        "ne" -> {x: x+1, y: y-1}
        "sw" -> {x: x-1, y: y+1}
        "se" -> {x: x+1, y: y+1}
        _ -> { x, y }

dist :: Xy -> Int
dist {x, y} | x < 0 = dist { x: (-x), y }
dist {x, y} | y < 0 = dist { x, y: (-y) }
dist {x, y} = max x y

step :: State -> String -> State
step state dir =
    let pos = follow state.pos dir
    in {
        pos,
        dist: max state.dist (dist pos)
    }

initialState = {pos: {x:0, y:0}, dist:0}

-- [part1, part2]
result input = [dist result.pos, result.dist]
    where result = foldl step initialState $ split (Pattern ",") input


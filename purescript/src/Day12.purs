module Day12 where
  
import Prelude

import Data.Array (foldl, head, take)
import Data.Maybe
import Data.Set as Set
import Data.String (split, trim, Pattern(..))
import Data.String.Utils as StringUtils
import Data.StrMap
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type ResultSet = Set.Set String

go = do
    input <- readTextFile UTF8 "src/Day12.txt"
    pure $ part2 input

part1 input = Set.size $ follow (toMap input) "0" Set.empty
part2 input = followAndRemove (toMap input) 0

followAndRemove pipeMap groups =
    case head (keys pipeMap) of
        Nothing -> groups
        Just firstKey -> 
            let
                rs = follow pipeMap firstKey Set.empty
                pm2 = filterKeys (\k -> not Set.member k rs) pipeMap
            in followAndRemove pm2 (groups + 1)

follow :: StrMap (Array String) -> String -> ResultSet -> ResultSet
follow pipeMap pipe resultSet =
    if Set.member pipe resultSet 
    then resultSet
    else follow2 pipeMap pipe $ Set.insert pipe resultSet

follow2 :: StrMap (Array String) -> String -> ResultSet -> ResultSet
follow2 pipeMap pipe resultSet =
    case lookup pipe pipeMap of
        Nothing -> resultSet
        Just ps -> foldl (\acc p -> follow pipeMap p acc) resultSet ps

toMap :: String -> StrMap (Array String)
toMap input = 
   foldl lineToMap empty $ StringUtils.lines input

lineToMap :: StrMap (Array String) -> String -> StrMap (Array String) 
lineToMap pipeMap line = 
    case split (Pattern "<->") line of
        [key, values] -> insert (trim key) (map trim $ split (Pattern ",") values) pipeMap
        _ -> pipeMap



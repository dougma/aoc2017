module Day10 where

import Data.Array (drop, foldl, length, range, reverse, slice, take)
import Data.Char
import Data.EuclideanRing
import Data.Int
import Data.Int.Bits
import Data.String as String
import Prelude

input1 = [189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62]
input2s = "189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62"
input2 = (map toCharCode $ String.toCharArray input2s) <> [17, 31, 73, 47, 23]

type State = { list :: Array Int, pos :: Int, skip :: Int }
initialState = { list: range 0 255, pos: 0, skip: 0 }

part1 = 
    let {list, pos} = foldl transform initialState input1
    in ror pos list

part2 =
    let
        iterate = \state _ -> foldl transform state input2
        {list, pos} = (foldl iterate initialState $ range 0 63)
    in
        dense $ ror pos list
    
dense :: Array Int -> String
dense a | length a == 0 = ""
dense a = 
    let hex = pad $ toStringAs hexadecimal $ foldl (.^.) 0 $ take 16 a
    in hex <> (dense $ drop 16 a)

pad s | String.length s == 2 = s
pad s = "0" <> s

transform :: State -> Int -> State
transform { list, pos, skip } x =
    {
        list : rol (x + skip) $ rev x list,
        pos : pos + (x + skip),
        skip : skip + 1
    }

-- reverse the front of the list
rev :: forall a. Int -> Array a -> Array a
rev a list | a < 2 = list
rev a list = p1 <> p2 where
    p1 = reverse $ take a list
    p2 = drop a list

-- rotate left (high indices move to low indices)
rol :: forall a. Int -> Array a -> Array a
rol a list = rol2 (a `mod` length list) list

rol2 a list | a == 0 = list
rol2 a list | a < 0 = rol2 (length list + a) list
rol2 a list = drop a list <> take a list

ror a list = rol (-1 * a) list

module Life where

import Dict exposing (Dict)
import Patterns

type alias Gen   = Dict Coord Cell
type alias Coord = (Int, Int)
type alias Cell  = {x: Int, y: Int, height: Int, width: Int, alive: Bool}


cells : Int -> Int -> List (Coord, Cell)
cells rows columns =
  [0..rows]
    |> List.concatMap
         (\y ->
            [0..columns]
              |> List.map
                 (\x ->
                    let alive = List.member (x,y) Patterns.horizontal3
                        x'     = (x * 10)  - 400
                        y'     = (-y * 10) + 400
                    in ((x, y) , (Cell x' y' 10 10 alive))))


neighbouringCoords : Coord -> List Coord
neighbouringCoords (x,y) =
  [(x - 1, y - 1),
   (x - 1, y),
   (x - 1, y + 1),
   (x,     y - 1),
   (x,     y + 1),
   (x + 1, y - 1),
   (x + 1, y),
   (x + 1, y + 1)]

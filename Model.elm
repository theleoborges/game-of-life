module Model where

import Dict exposing (Dict)
import Set  exposing (Set)

type alias Model = { generation  : Gen,
                     livingCells : Set Coord}

type alias Gen   = Dict Coord Cell
type alias Coord = (Int, Int)
type alias Cell  = {x: Int, y: Int, height: Int, width: Int, alive: Bool}



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

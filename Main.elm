module Main where

import Array exposing (Array, repeat, initialize)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random
import Debug exposing (log)
import Time exposing (Time)
import Dict exposing (Dict)
import Maybe

-- MODEL

type alias Model =
    { generation : Dict Coord Cell }

type alias Coord = (Int, Int)

type alias Cell = {x: Int, y: Int, height: Int, width: Int, alive: Bool}


init rows columns =
  cells' rows columns
    |> Dict.fromList
    |> Model


cells' rows columns =
  [0..rows]
    |> List.concatMap
         (\y ->
            [0..columns]
              |> List.map
                 (\x ->
                    let alive = ((x % 5 == 0) || (y % 5 == 0))
                    in ((x, y) , (Cell (x * 10) (y * 10) 10 10 alive))))

-- UPDATE
numAliveNeighbours dict (x,y) cell =
  let neighbours = [(x - 1, y - 1),
                    (x - 1, y),
                    (x - 1, y + 1),
                    (x,     y - 1),
                    (x,     y + 1),
                    (x + 1, y - 1),
                    (x + 1, y),
                    (x + 1, y + 1)]
      livingStatus coord =
        case Dict.get coord dict of
          (Just cell) -> cell.alive
          Nothing     -> False
  in List.map livingStatus neighbours
      |> List.foldr (\status acc -> if status then acc + 1 else acc) 0

handleLivingCell dict coord cell =
  let n = numAliveNeighbours dict coord cell
  in if n < 2 || n > 3
     then {cell | alive = False}
     else cell

handleDeadCell dict coord cell =
  let n = numAliveNeighbours dict coord cell
  in if n == 3
     then {cell | alive = True}
     else cell

handleCell : (Dict Coord Cell) -> Coord -> Cell -> Cell
handleCell dict coord cell =
  case cell.alive of
    True  -> handleLivingCell dict coord cell
    False -> handleDeadCell  dict coord cell

update : Time -> Model -> Model
update _ model =
  let folder coord cell dict =
        Dict.insert coord (handleCell dict coord cell) dict
      generation' = Dict.foldl folder model.generation model.generation
  in
  { model | generation = generation' }



-- VIEW

cell : Cell -> Form
cell {x, y, height, width, alive} =
  rect (toFloat width) (toFloat height)
    |> filled (if alive then black else white)
    |> move (toFloat x, toFloat y)


view : Model -> Element
view {generation} =
  List.map cell (Dict.values generation)
    |> collage 800 800



-- main : Element
-- main =
--   List.map view (cells 10 10)
--     |> collage 600 600

main : Signal Element
main =
  Signal.map view (Signal.foldp update (init 30 30) (Time.fps 5))

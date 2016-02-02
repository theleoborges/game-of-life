module Main where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (Time)
import Dict exposing (Dict)

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
--                    let alive = x % 9 == 0 || x % 3 /= 0
                    let alive = List.member (x,y) grower

                        x'     = x * 10
                        y'     = -y * 10
                    in ((x, y) , (Cell x' y' 10 10 alive))))

glider = [(11,0), (12,1), (10,2), (11,2), (12,2)]
gliderGun = [(3,7),(4,7),(3,8),(4,8),(13,7),(13,8),(13,9),(14,6),(14,10),(15,5),(16,5),(15,11),(16,11),(17,8),(18,6),(18,10),(19,7),(19,8),(19,9),(20,8),(23,5),(24,5),(23,6),(24,6),(23,7), (24,7),(25,4),(25,8), (27,3), (27,4),(27,8),(27,9),(37,5),(37,6),(38,5),(38,6)]
grower = [(20,30),(22,30),(22,29),(24,28),(24,27),(24,26),(26,27),(26,26),(26,25),(27,26)]
-- UPDATE

update : Time -> Model -> Model
--update _ model = Debug.watch "model" model
update _ model =
  let folder coord cell dict =
        Dict.insert coord (handleCell model.generation coord cell) dict
      generation' = Dict.foldl folder model.generation model.generation
  in
  { model | generation = generation' }


numAliveNeighbours dict (x,y) =
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
      |> List.foldl (\status acc -> if status then acc + 1 else acc) 0

handleLivingCell dict coord cell =
  let n = numAliveNeighbours dict coord
  in if n < 2 || n > 3
     then {cell | alive = False}
     else cell

handleDeadCell dict coord cell =
  let n = numAliveNeighbours dict coord
  in if n == 3
     then {cell | alive = True}
     else cell

handleCell : (Dict Coord Cell) -> Coord -> Cell -> Cell
handleCell dict coord cell =
  case cell.alive of
    True  -> handleLivingCell dict coord cell
    False -> handleDeadCell  dict coord cell


-- VIEW

view : Model -> Element
view {generation} =
  List.map cell (Dict.values generation)
    |> collage 800 800

cell : Cell -> Form
cell {x, y, height, width, alive} =
  rect (toFloat width) (toFloat height)
    |> filled (if alive then black else white)
    |> move (toFloat x, toFloat y)


-- MAIN

main : Signal Element
main =
  Signal.map view (Signal.foldp update (init 40 40) (Time.fps 10))

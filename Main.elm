module Main where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (Time)
import Dict exposing (Dict)
import Patterns
import Maybe exposing (withDefault)
import Set exposing (Set)

-- MODEL

type alias Model = { generation  : Dict Coord Cell,
                     livingCells : Set Coord}
type alias Coord = (Int, Int)
type alias Cell  = {x: Int, y: Int, height: Int, width: Int, alive: Bool}


init rows columns =
  let cells'      =  cells rows columns
      livingCells =
        List.filter (.alive << snd) cells'
          |> List.map fst
          |> List.concatMap (\coord -> coord :: neighbouringCoords coord)
          |> Set.fromList
  in  Model (Dict.fromList cells') (Debug.log "lc" livingCells)

cells rows columns =
  [0..rows]
    |> List.concatMap
         (\y ->
            [0..columns]
              |> List.map
                 (\x ->
--                    let alive = x % 9 == 0 || x % 3 /= 0
                    let alive = List.member (x,y) Patterns.gliderGun

                        x'     = (x * 10)  - 400
                        y'     = (-y * 10) + 400
                    in ((x, y) , (Cell x' y' 10 10 alive))))

-- UPDATE

-- update : Time -> Model -> Model
-- update _ model =
--   let folder coord cell dict =
--         Dict.insert coord (handleCell model.generation coord cell) dict
--       generation' = Dict.foldr folder model.generation model.generation
--   in
--   { model | generation = generation' }


update : Time -> Model -> Model
update _ model =
  let folder coord (newGen, newLivingCells) =
        let newCell =
              Dict.get coord model.generation
                |> Maybe.map (\cell -> (cell, handleCell model.generation coord cell))
        in case newCell of
             (Just (old, new)) -> ( Dict.insert coord new newGen,
                                    if old.alive /= new.alive
                                    then Set.union newLivingCells (Set.fromList <| coord :: neighbouringCoords coord)
                                    else newLivingCells
--                                    else newModel.livingCells
                                  )

             Nothing  -> (newGen, newLivingCells)
--  in List.foldr folder model (Dict.keys model.generation)
      (newGen, newLivingCells) = Set.foldr folder (model.generation, Set.empty) model.livingCells
  in { model | generation = newGen, livingCells = newLivingCells }

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

numAliveNeighbours dict (x,y) =
  let neighbours = [(x - 1, y - 1),
                    (x - 1, y),
                    (x - 1, y + 1),
                    (x,     y - 1),
                    (x,     y + 1),
                    (x + 1, y - 1),
                    (x + 1, y),
                    (x + 1, y + 1)]
      livingStatus coord acc =
        Dict.get coord dict
          |> Maybe.map (\cell -> if cell.alive then (acc + 1) else acc)
          |> withDefault acc
  in List.foldr livingStatus 0 neighbours

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
  Signal.map view (Signal.foldp update (init 100 100) (Time.fps 20))

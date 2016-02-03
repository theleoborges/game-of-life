module Main where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (Time)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Set exposing (Set)
import Patterns
import Utils exposing (ap)

-- MODEL

type alias Model = { generation  : Gen,
                     livingCells : Set Coord}

type alias Gen   = Dict Coord Cell
type alias Coord = (Int, Int)
type alias Cell  = {x: Int, y: Int, height: Int, width: Int, alive: Bool}


init rows columns =
  let cells'      =  cells rows columns
      livingCells =
        List.filter (.alive << snd) cells'
          |> List.map fst
          |> List.concatMap (ap (::) neighbouringCoords)
          |> Set.fromList
  in  Model (Dict.fromList cells') livingCells

cells rows columns =
  [0..rows]
    |> List.concatMap
         (\y ->
            [0..columns]
              |> List.map
                 (\x ->
                    let alive = List.member (x,y) Patterns.gliderGun
                        x'     = (x * 10)  - 400
                        y'     = (-y * 10) + 400
                    in ((x, y) , (Cell x' y' 10 10 alive))))

-- UPDATE
update : Time -> Model -> Model
update _ model =
  let (generation', livingCells') =
        Set.foldr (advanceGeneration model.generation) (model.generation, []) model.livingCells
  in { model | generation  = generation',
               livingCells = Set.fromList livingCells' }

advanceGeneration : Gen
                    -> Coord
                    -> (Gen, List Coord)
                    -> (Gen, List Coord)
advanceGeneration generation coord (newGen, newLivingCells) =
        let newCell = Dict.get coord generation
                        |> Maybe.map (ap (,) (handleCell generation coord))
        in case newCell of
             (Just (old, new)) -> ( Dict.insert coord new newGen,
                                    if old.alive /= new.alive
                                    then List.append newLivingCells <| coord :: neighbouringCoords coord
                                    else newLivingCells
                                  )

             Nothing  -> (newGen, newLivingCells)


handleCell : Gen -> Coord -> Cell -> Cell
handleCell dict coord cell =
  let n = numAliveNeighbours dict coord
  in case cell.alive of
       True  -> toggleIf (\n -> n < 2 || n > 3) cell n
       False -> toggleIf ((==) 3)               cell n

toggleIf pred cell n =
  if pred n
     then {cell | alive = not cell.alive}
     else cell

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

numAliveNeighbours dict coord =
  let neighbours = neighbouringCoords coord
      livingStatus coord acc =
        Dict.get coord dict
          |> Maybe.map (\cell -> if cell.alive then (acc + 1) else acc)
          |> withDefault acc
  in List.foldr livingStatus 0 neighbours

-- VIEW

view : Model -> Element
view {generation, livingCells} =
  let cellsToRender coord cells =
        case Dict.get coord generation of
          (Just cell) -> (cellView cell) :: cells
          Nothing     -> cells
  in
    Set.foldr cellsToRender [] livingCells
      |> collage 800 800

cellView : Cell -> Form
cellView {x, y, height, width, alive} =
  rect (toFloat width) (toFloat height)
    |> filled (if alive then black else white)
    |> move (toFloat x, toFloat y)


-- MAIN

main : Signal Element
main =
  Signal.map view (Signal.foldp update (init 100 100) (Time.fps 20))

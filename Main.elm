module Main exposing (..)

import Color            exposing (..)
import Collage          exposing (collage, move, filled, Form, rect)
import Element          exposing (..)
import Time             exposing (Time)
import Dict             exposing (Dict)
import Maybe            exposing (withDefault)
import Set              exposing (Set)
import Patterns         exposing (Pattern, getPattern)
import Utils            exposing (ap, grid2d)
import Html             exposing (..)
import Html.App         exposing (..)
import Html.Attributes  exposing (..)
import Html.Events      exposing (on, targetChecked)
import VirtualDom       exposing (Node)
import Json.Decode      as Json
import AnimationFrame   exposing (..)
import Platform.Cmd     as Cmd exposing (Cmd)

-- MODEL

type alias Model = { generation  : Gen,
                     livingCells : Set Coord,
                     config      : Config}

type alias Gen     = Dict Coord Cell
type alias Coord   = (Int, Int)
type alias Cell    = {x: Int, y: Int, height: Int, width: Int, alive: Bool}
type alias Config  = {rows: Int, columns: Int, height: Float, width: Float}

init : Config -> Pattern -> Model
init config pattern =
  let cells'      =  cells config pattern
      livingCells =
        List.filter (.alive << snd) cells'
          |> List.map fst
          |> List.concatMap (ap (::) neighbouringCoords)
          |> Set.fromList
  in  Model (Dict.fromList cells') livingCells config

cells : Config -> Pattern -> List (Coord, Cell)
cells {rows, columns, height, width} pattern =
  let makeCell (x, y) =
        let alive = List.member (x,y) pattern
            x'     = (x * 10)  - (round (width / 2))
            y'     = (-y * 10) + (round (height / 2))
        in ((x, y) , (Cell x' y' 10 10 alive))
  in grid2d rows columns
       |> List.map makeCell



-- UPDATE
type Action = AdvanceGeneration
            | Restart PatternName

type alias PatternName = String

update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    AdvanceGeneration ->
      let (generation', livingCells') =
            Set.foldr (advanceGeneration model.generation) (model.generation, []) model.livingCells
      in ({ model | generation  = generation',
                    livingCells = Set.fromList livingCells' }, Cmd.none)

    (Restart name) -> (init model.config <| getPattern name, Cmd.none)

advanceGeneration : Gen
                    -> Coord
                    -> (Gen, List Coord)
                    -> (Gen, List Coord)
advanceGeneration generation coord (newGen, newLivingCells) =
        let updateModel (old, new) = ( Dict.insert coord new newGen,
                                       if old.alive /= new.alive
                                       then List.append newLivingCells <| coord :: neighbouringCoords coord
                                       else newLivingCells
                                     )
         in Dict.get coord generation
              |> Maybe.map (ap (,) (updateCell generation coord))
              |> Maybe.map updateModel
              |> withDefault (newGen, newLivingCells)


updateCell : Gen -> Coord -> Cell -> Cell
updateCell dict coord cell =
  let n = numAliveNeighbours dict coord
  in case cell.alive of
       True  -> toggleIf (\n -> n < 2 || n > 3) cell n
       False -> toggleIf ((==) 3)               cell n

toggleIf : (Int -> Bool) -> Cell -> Int -> Cell
toggleIf pred cell n =
  if pred n
     then {cell | alive = not cell.alive}
     else cell

numAliveNeighbours : Gen -> Coord -> Int
numAliveNeighbours dict coord =
  let neighbours = neighbouringCoords coord
      livingStatus coord acc =
        Dict.get coord dict
          |> Maybe.map (\cell -> if cell.alive then (acc + 1) else acc)
          |> withDefault acc
  in List.foldr livingStatus 0 neighbours

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

-- VIEW

view : Model -> Node Action
view {generation, livingCells, config} =
  let view' coord cells =
        Dict.get coord generation
          |> Maybe.map ((flip (::) cells) << cellView)
          |> withDefault cells
      grid = Set.foldr view' [] livingCells
               |> collage (round config.width) (round config.height)
               |> toHtml
  in
    div []
      <| options ++ [grid]

cellView : Cell -> Form
cellView {x, y, height, width, alive} =
  rect (toFloat width) (toFloat height)
    |> filled (if alive then black else white)
    |> move (toFloat x, toFloat y)

options : List (Html Action)
options =
  List.concatMap radio ["glider", "gliderGun", "grower", "dieHard", "horizontal"]

radio : String -> List (Html Action)
radio key =
  [ input [type' "radio",
           name  "pattern",
           value key,
           on    "change" (Json.succeed (Restart key))
          ]
          []
  , text key
  ]

-- SUBSCRIPTIONS

subscriptions   : Model -> Sub Action
subscriptions _ = AnimationFrame.diffs (always AdvanceGeneration)

-- MAIN
main : Program Never
main =
  let init' = init (Config 100 100 800 800) Patterns.gliderGun2
  in program
       { init          = (init', Cmd.none)
       , view          = view
       , update        = update
       , subscriptions = subscriptions
       }

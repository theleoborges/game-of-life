module Patterns exposing (..)

type alias Pattern = List ( Int, Int )


glider = [(11,0), (12,1), (10,2), (11,2), (12,2)]
gliderGun = [(3,7),(4,7),(3,8),(4,8),(13,7),(13,8),(13,9),(14,6),(14,10),(15,5),(16,5),(15,11),(16,11),(17,8),(18,6),(18,10),(19,7),(19,8),(19,9),(20,8),(23,5),(24,5),(23,6),(24,6),(23,7), (24,7),(25,4),(25,8), (27,3), (27,4),(27,8),(27,9),(37,5),(37,6),(38,5),(38,6)]
grower = [(20,30),(22,30),(22,29),(24,28),(24,27),(24,26),(26,27),(26,26),(26,25),(27,26)]
dieHard = [(19,25),(20,25),(20,26),(24,26),(25,26),(26,26),(25,24)]
horizontal = [(5,25),(6,25),(7,25),(8,25),(9,25),(10,25),(11,25),(12,25),(14,25),(15,25),(16,25),(17,25),(18,25),(22,25),(23,25),(24,25),(31,25),(32,25),(33,25),(34,25),(35,25),(36,25),(38,25),(39,25),(40,25),(41,25),(42,25)]

glider2 = List.concatMap
            (\(x,y) -> [(x,y), (x + 10, y + 10)])
            glider

grower2 = List.concatMap
            (\(x,y) -> [(x,y), (x + 30, y + 30)])
            grower

grower3 = List.concatMap
            (\(x,y) -> [(x,y), (x - 30, y + 10)])
            grower2

dieHard2 = List.concatMap
             (\(x,y) -> [(x,y), (x + 10, y + 10)])
             dieHard

dieHard3 = List.concatMap
             (\(x,y) -> [(x,y), (x - 10, y - 10)])
             dieHard2

gliderGun2 = List.concatMap
               (\(x,y) -> [(x,y), (x + 30, y + 30)])
               gliderGun

horizontal2 = List.concatMap
                (\(x,y) -> [(x,y), (x + 20, y + 10)])
                horizontal

horizontal3 = List.concatMap
                (\(x,y) -> [(x,y), (x + 20, y + 10)])
                  horizontal2


getPattern : String -> Pattern
getPattern name =
  case name of
    "glider"     -> glider
    "gliderGun"  -> gliderGun
    "grower"     -> grower
    "dieHard"    -> dieHard
    "horizontal" -> horizontal
    _            -> glider

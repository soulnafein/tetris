module Rotation exposing (Rotation(..), next)


type Rotation
    = North
    | East
    | South
    | West


next : Rotation -> Rotation
next rotation =
    case rotation of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North

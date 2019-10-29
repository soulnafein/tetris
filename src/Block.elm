module Block exposing
    ( Block
    , create
    , update
    )

import Configuration exposing (gridWidth, squareSize)
import List exposing (..)
import Palette exposing (..)
import Rotation exposing (Rotation(..))
import TetrominoType exposing (TetrominoType(..))


type alias Block =
    { x : Float
    , y : Float
    , color : String
    }


create block =
    { x = block.x
    , y = block.y
    , color = block.color
    }


update ( blocks, points ) =
    let
        lineToDelete =
            nextLineToDelete blocks
    in
    case lineToDelete of
        Nothing ->
            ( blocks, points )

        Just y ->
            update ( removeLine y blocks, 50 + points )


nextLineToDelete blocks =
    let
        lines =
            blocks |> List.map .y
    in
    lines
        |> List.filter (lineFull blocks)
        |> List.head


lineFull blocks y =
    let
        numberOfBlocksInLine =
            blocks
                |> List.filter (\b -> b.y == y)
                |> List.length
    in
    numberOfBlocksInLine == gridWidth


removeLine y blocks =
    let
        blocksToMove =
            blocks
                |> List.filter (\b -> b.y < y)
                |> List.map (\b -> { b | y = b.y + squareSize })

        blocksToLeave =
            blocks
                |> List.filter (\b -> b.y > y)
    in
    blocksToMove ++ blocksToLeave

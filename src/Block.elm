module Block exposing
    ( Block
    , create
    , update
    )

import Configuration exposing (gridWidth, squareSize)
import List exposing (filter, head, length, map)
import Rotation exposing (Rotation(..))
import TetrominoType exposing (TetrominoType(..))


type alias Block =
    { x : Float
    , y : Float
    , color : String
    }


create : Block -> Block
create block =
    { x = block.x
    , y = block.y
    , color = block.color
    }


update : ( List Block, Int ) -> ( List Block, Int )
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


nextLineToDelete : List Block -> Maybe Float
nextLineToDelete blocks =
    let
        lines =
            blocks |> List.map .y
    in
    lines
        |> List.filter (lineFull blocks)
        |> List.head


lineFull : List Block -> Float -> Bool
lineFull blocks y =
    let
        numberOfBlocksInLine =
            blocks
                |> List.filter (\b -> b.y == y)
                |> List.length
    in
    numberOfBlocksInLine == gridWidth


removeLine : Float -> List Block -> List Block
removeLine y blocks =
    let
        blocksToMove =
            blocks
                |> List.filter (\b -> b.y < y)
                |> List.map (\b -> { b | y = b.y + toFloat squareSize })

        blocksToLeave =
            blocks
                |> List.filter (\b -> b.y > y)
    in
    blocksToMove ++ blocksToLeave

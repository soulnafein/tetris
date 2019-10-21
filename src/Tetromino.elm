module Tetromino exposing (Tetromino, TetrominoType(..), create, createList, update)

import Configuration
    exposing
        ( fallingSpeed
        , movingSpeed
        , squareSize
        )


type TetrominoType
    = I
    | J
    | L
    | O
    | S
    | Z
    | T


type alias Tetromino =
    { x : Float
    , y : Float
    , actualX : Float
    , tetrominoType : TetrominoType
    }


create params =
    { x = params.x
    , y = params.y
    , actualX = params.x
    , tetrominoType = params.tetrominoType
    }


createList listParams =
    listParams
        |> List.map create


update delta keyboard tetromino =
    let
        horizontalSpeed =
            if keyboard.leftArrowPressed then
                -movingSpeed

            else if keyboard.rightArrowPressed then
                movingSpeed

            else
                0

        updatedY =
            tetromino.y + (fallingSpeed * delta)

        updatedActualX =
            tetromino.actualX + (horizontalSpeed * delta)

        updatedX =
            toFloat (floor (updatedActualX / squareSize) * squareSize)
    in
    { tetromino | y = updatedY, x = updatedX, actualX = updatedActualX }

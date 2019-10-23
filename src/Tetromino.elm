module Tetromino exposing
    ( Tetromino
    , TetrominoType(..)
    , create
    , createList
    , generateRandomType
    , reachedBottom
    , update
    )

import Configuration
    exposing
        ( backgroundHeight
        , fallingSpeed
        , movingSpeed
        , squareSize
        )
import Random


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
    , blocks : List Block
    }


type alias Block =
    { x : Float
    , y : Float
    }


create params =
    { x = params.x
    , y = params.y
    , actualX = params.x
    , tetrominoType = params.tetrominoType
    , blocks = createBlocks params
    }


createList listParams =
    listParams
        |> List.map create


update delta keyboard tetromino =
    let
        horizontalSpeed =
            calculateHorizontalSpeed keyboard
    in
    tetromino
        |> updateY delta
        |> updateX delta horizontalSpeed
        |> updateBlocks


updateY delta tetromino =
    { tetromino | y = tetromino.y + (fallingSpeed * delta) }


updateX delta horizontalSpeed tetromino =
    let
        updatedActualX =
            tetromino.actualX + (horizontalSpeed * delta)

        updatedX =
            toFloat (floor (updatedActualX / squareSize) * squareSize)
    in
    { tetromino | x = updatedX, actualX = updatedActualX }


updateBlocks tetromino =
    { tetromino | blocks = createBlocks tetromino }


calculateHorizontalSpeed keyboard =
    if keyboard.leftArrowPressed then
        -movingSpeed

    else if keyboard.rightArrowPressed then
        movingSpeed

    else
        0


reachedBottom tetromino =
    tetromino.y > backgroundHeight


generateRandomType msg =
    Random.generate msg randomTetrominoGenerator


randomTetrominoGenerator =
    Random.uniform I [ J, L, O, S, Z, T ]


createBlocks params =
    let
        blocks =
            case params.tetrominoType of
                I ->
                    blocksI

                J ->
                    blocksJ

                L ->
                    blocksL

                O ->
                    blocksO

                S ->
                    blocksS

                Z ->
                    blocksZ

                T ->
                    blocksT
    in
    blocks
        |> List.map (convertCoordinates params.x params.y)


blocksI =
    [ { x = 0, y = 0 }
    , { x = 1, y = 0 }
    , { x = 2, y = 0 }
    , { x = 3, y = 0 }
    ]


blocksJ =
    [ { x = 0, y = 0 }
    , { x = 0, y = 1 }
    , { x = 1, y = 1 }
    , { x = 2, y = 1 }
    ]


blocksL =
    [ { x = 2, y = 0 }
    , { x = 0, y = 1 }
    , { x = 1, y = 1 }
    , { x = 2, y = 1 }
    ]


blocksO =
    [ { x = 0, y = 0 }
    , { x = 1, y = 0 }
    , { x = 0, y = 1 }
    , { x = 1, y = 1 }
    ]


blocksS =
    [ { x = 2, y = 0 }
    , { x = 1, y = 0 }
    , { x = 1, y = 1 }
    , { x = 0, y = 1 }
    ]


blocksZ =
    [ { x = 0, y = 0 }
    , { x = 1, y = 0 }
    , { x = 1, y = 1 }
    , { x = 2, y = 1 }
    ]


blocksT =
    [ { x = 0, y = 1 }
    , { x = 1, y = 1 }
    , { x = 2, y = 1 }
    , { x = 1, y = 0 }
    ]


convertCoordinates x y block =
    { block
        | x = block.x * squareSize + x
        , y = block.y * squareSize + y
    }

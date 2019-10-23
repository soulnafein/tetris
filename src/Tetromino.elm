module Tetromino exposing
    ( Tetromino
    , TetrominoType(..)
    , create
    , createList
    , generateRandomType
    , stoppedMoving
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
    , previousActualX : Float
    , previousY : Float
    , actualX : Float
    , tetrominoType : TetrominoType
    , blocks : List Block
    , verticalSpeed : Int
    }


type alias Block =
    { x : Float
    , y : Float
    }


create params =
    { x = params.x
    , y = params.y
    , actualX = params.x
    , previousActualX = params.x
    , previousY = params.y
    , tetrominoType = params.tetrominoType
    , blocks = createBlocks params
    , verticalSpeed = fallingSpeed
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
        |> updatePreviousPosition
        |> fall delta
        |> updateX delta horizontalSpeed
        |> resolveBottomCollision
        |> updateBlocks


updatePreviousPosition tetromino =
    { tetromino | previousY = tetromino.y, previousActualX = tetromino.actualX }


fall delta tetromino =
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


resolveBottomCollision tetromino =
    let
        verticalCollisionDistance =
            lowestBlockY tetromino - backgroundHeight
    in
    if verticalCollisionDistance > 0 then
        tetromino
            |> updateY (tetromino.y - verticalCollisionDistance)
            |> updateVerticalSpeed 0

    else
        tetromino


updateY newY tetromino =
    { tetromino | previousY = tetromino.y, y = newY }


updateVerticalSpeed speed tetromino =
    { tetromino | verticalSpeed = speed }


stoppedMoving tetromino =
    tetromino.verticalSpeed == 0


reachedBottom tetromino =
    tetromino.y > backgroundHeight


lowestBlockY : Tetromino -> Float
lowestBlockY tetromino =
    tetromino.blocks
        |> List.map .y
        |> List.map (\y -> y + squareSize)
        |> List.sort
        |> List.reverse
        |> List.head
        |> Maybe.withDefault 0



-- any blocks reached bottom


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

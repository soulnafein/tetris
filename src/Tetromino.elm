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
        , backgroundWidth
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
    , actualY : Float
    , tetrominoType : TetrominoType
    , blocks : List Block
    , verticalSpeed : Int
    , rotation : Rotation
    , hasJustRotated : Bool
    }


type alias Block =
    { x : Float
    , y : Float
    }


type Rotation
    = North
    | East
    | South
    | West


create params =
    { x = params.x
    , y = params.y
    , actualX = params.x
    , actualY = params.y
    , tetrominoType = params.tetrominoType
    , verticalSpeed = fallingSpeed
    , rotation = East
    , hasJustRotated = False
    , blocks = []
    }
        |> updateBlocks


createList listParams =
    listParams
        |> List.map create


update delta keyboard tetromino =
    let
        horizontalSpeed =
            calculateHorizontalSpeed keyboard
    in
    tetromino
        |> fall delta
        |> horizontalMovement delta horizontalSpeed
        |> updateBlocks
        |> resolveCollisions
        |> updateBlocks
        |> updateRotation keyboard


updateRotation keyboard tetromino =
    let
        rotation =
            if keyboard.upArrowPressed && not tetromino.hasJustRotated then
                nextRotation tetromino.rotation

            else
                tetromino.rotation

        hasJustRotated =
            keyboard.upArrowPressed
    in
    { tetromino | rotation = rotation, hasJustRotated = hasJustRotated }


nextRotation rotation =
    case rotation of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


fall delta tetromino =
    tetromino
        |> updateY (tetromino.actualY + (fallingSpeed * delta))


stickToGrid aNumber =
    toFloat (floor (aNumber / squareSize) * squareSize)


updateY actualY tetromino =
    { tetromino | actualY = actualY, y = stickToGrid actualY }


horizontalMovement delta horizontalSpeed tetromino =
    let
        updatedActualX =
            tetromino.actualX + (horizontalSpeed * delta)
    in
    updateX updatedActualX tetromino


updateX actualX tetromino =
    { tetromino | actualX = actualX, x = stickToGrid actualX }


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
            |> updateY (tetromino.actualY - verticalCollisionDistance)
            |> updateVerticalSpeed 0

    else
        tetromino


resolveCollisions tetromino =
    tetromino
        |> resolveBottomCollision
        |> resolveLeftCollision
        |> resolveRightCollision


resolveLeftCollision tetromino =
    let
        leftCollisionDistance =
            leftmostBlockX tetromino
    in
    if leftCollisionDistance < 0 then
        let
            updatedTetromino =
                tetromino
                    |> updateX (tetromino.actualX - leftCollisionDistance)
        in
        updatedTetromino

    else
        tetromino


resolveRightCollision tetromino =
    let
        rightCollisionDistance =
            rightmostBlockX tetromino - backgroundWidth
    in
    if rightCollisionDistance > 0 then
        let
            updatedTetromino =
                tetromino
                    |> updateX (tetromino.actualX - rightCollisionDistance)
        in
        updatedTetromino

    else
        tetromino


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


leftmostBlockX : Tetromino -> Float
leftmostBlockX tetromino =
    tetromino.blocks
        |> List.map .x
        |> List.sort
        |> List.head
        |> Maybe.withDefault 0


rightmostBlockX : Tetromino -> Float
rightmostBlockX tetromino =
    tetromino.blocks
        |> List.map .x
        |> List.map (\x -> x + squareSize)
        |> List.sort
        |> List.reverse
        |> List.head
        |> Maybe.withDefault 0



-- any blocks reached bottom


generateRandomType msg =
    Random.generate msg randomTetrominoGenerator


randomTetrominoGenerator =
    Random.uniform I [ J, L, O, S, Z, T ]


updateBlocks tetromino =
    let
        blocksFunction =
            case tetromino.tetrominoType of
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

        blocks =
            blocksFunction tetromino.rotation
                |> stringsToBlocks
                |> List.map (convertCoordinates tetromino.x tetromino.y)
    in
    { tetromino | blocks = blocks }


blocksI rotation =
    case rotation of
        North ->
            [ "OOOO"
            , "####"
            ]

        East ->
            [ "OO#O"
            , "OO#O"
            , "OO#O"
            , "OO#O"
            ]

        South ->
            [ "OOOO"
            , "OOOO"
            , "####"
            ]

        West ->
            [ "O#OO"
            , "O#OO"
            , "O#OO"
            , "O#OO"
            ]


blocksJ rotation =
    case rotation of
        North ->
            [ "#OOO"
            , "###O"
            ]

        East ->
            [ "O##O"
            , "O#OO"
            , "O#OO"
            ]

        South ->
            [ "OOOO"
            , "###O"
            , "OO#O"
            ]

        West ->
            [ "O#OO"
            , "O#OO"
            , "##OO"
            ]


blocksL rotation =
    case rotation of
        North ->
            [ "OO#O"
            , "###O"
            ]

        East ->
            [ "O#OO"
            , "O#OO"
            , "O##O"
            ]

        South ->
            [ "OOOO"
            , "###O"
            , "#OOO"
            ]

        West ->
            [ "##OO"
            , "O#OO"
            , "O#OO"
            ]


blocksO rotation =
    case rotation of
        _ ->
            [ "0##0"
            , "0##0"
            ]


blocksS rotation =
    case rotation of
        North ->
            [ "O##O"
            , "##OO"
            ]

        East ->
            [ "O#OO"
            , "O##O"
            , "OO#O"
            ]

        South ->
            [ "OOOO"
            , "O##O"
            , "##OO"
            ]

        West ->
            [ "#OOO"
            , "##OO"
            , "O#OO"
            ]


blocksZ rotation =
    case rotation of
        North ->
            [ "##OO"
            , "O##O"
            ]

        East ->
            [ "OO#O"
            , "O##O"
            , "O#OO"
            ]

        South ->
            [ "OOOO"
            , "##OO"
            , "O##O"
            ]

        West ->
            [ "O#OO"
            , "##OO"
            , "#OOO"
            ]


blocksT rotation =
    case rotation of
        _ ->
            [ "O#OO"
            , "###O"
            ]


convertCoordinates x y block =
    { block
        | x = block.x * squareSize + x
        , y = block.y * squareSize + y
    }


stringsToBlocks rows =
    List.indexedMap stringToBlocks rows
        |> List.concat


stringToBlocks y string =
    string
        |> String.indexes "#"
        |> List.map (\x -> { x = toFloat x, y = toFloat y })

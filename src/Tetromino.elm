module Tetromino exposing
    ( Tetromino
    , create
    , createList
    , generateRandomType
    , stoppedMoving
    , update
    )

import Block exposing (Block)
import Configuration
    exposing
        ( backgroundHeight
        , backgroundWidth
        , fallingSpeed
        , movingSpeed
        , squareSize
        )
import Random
import Rotation exposing (Rotation(..))
import TetrominoType exposing (TetrominoType(..))


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
                Rotation.next tetromino.rotation

            else
                tetromino.rotation

        hasJustRotated =
            keyboard.upArrowPressed
    in
    { tetromino | rotation = rotation, hasJustRotated = hasJustRotated }


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


generateRandomType msg =
    Random.generate msg randomTetrominoGenerator


randomTetrominoGenerator =
    Random.uniform I [ J, L, O, S, Z, T ]


updateBlocks tetromino =
    let
        blocks =
            Block.createByType
                tetromino.tetrominoType
                tetromino.rotation
                tetromino.x
                tetromino.y
    in
    { tetromino | blocks = blocks }

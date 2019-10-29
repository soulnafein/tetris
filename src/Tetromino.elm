module Tetromino exposing
    ( Tetromino
    , generateRandomType
    , init
    , stoppedMoving
    , update
    )

import Block exposing (Block)
import BlockFactory
import Collisions
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
    , verticalSpeed : Float
    , rotation : Rotation
    , hasJustRotated : Bool
    }


init tetrominoType =
    { x = 0
    , y = 0
    , actualX = 0
    , actualY = 0
    , tetrominoType = tetrominoType
    , verticalSpeed = fallingSpeed
    , rotation = North
    , hasJustRotated = False
    , blocks = []
    }
        |> updateBlocks


updateY actualY tetromino =
    { tetromino | actualY = actualY, y = stickToGrid actualY }
        |> updateBlocks


updateX actualX tetromino =
    { tetromino | actualX = actualX, x = stickToGrid actualX }
        |> updateBlocks


update delta keyboard tetromino blocks score =
    tetromino
        |> updateFallingSpeed keyboard (toFloat score)
        |> fall delta
        |> resolveVerticalCollisions blocks
        |> horizontalMovement delta keyboard
        |> resolveHorizontalCollisions blocks keyboard
        |> updateRotation keyboard blocks


updateFallingSpeed keyboard score tetromino =
    let
        modifiedSpeed =
            fallingSpeed * (1 + (score / 700))

        speed =
            if keyboard.bottomArrowPressed then
                modifiedSpeed + (5 * fallingSpeed)

            else
                modifiedSpeed
    in
    updateVerticalSpeed speed tetromino


updateRotation keyboard blocks tetromino =
    let
        rotation =
            if keyboard.upArrowPressed && not tetromino.hasJustRotated then
                Rotation.next tetromino.rotation

            else
                tetromino.rotation

        hasJustRotated =
            keyboard.upArrowPressed

        updatedTetromino =
            { tetromino | rotation = rotation, hasJustRotated = hasJustRotated }
                |> updateBlocks
    in
    if Collisions.horizontally blocks updatedTetromino then
        tetromino

    else
        updatedTetromino


fall delta tetromino =
    tetromino
        |> updateY (tetromino.actualY + (tetromino.verticalSpeed * delta))


stickToGrid aNumber =
    toFloat (floor (aNumber / squareSize) * squareSize)


horizontalMovement delta keyboard tetromino =
    let
        horizontalSpeed =
            calculateHorizontalSpeed keyboard

        updatedActualX =
            tetromino.actualX + (horizontalSpeed * delta)
    in
    tetromino
        |> updateX updatedActualX
        |> updateBlocks


calculateHorizontalSpeed keyboard =
    if keyboard.leftArrowPressed then
        -movingSpeed

    else if keyboard.rightArrowPressed then
        movingSpeed

    else
        0


resolveVerticalCollisions blocks tetromino =
    if Collisions.bottomScreen tetromino || Collisions.withBlocks tetromino blocks then
        tetromino
            |> updateY (tetromino.y - squareSize)
            |> updateVerticalSpeed 0

    else
        tetromino


resolveHorizontalCollisions blocks keyboard tetromino =
    tetromino
        |> resolveLeftCollision blocks keyboard
        |> resolveRightCollision blocks keyboard


resolveLeftCollision blocks keyboard tetromino =
    if keyboard.leftArrowPressed then
        if Collisions.leftScreen tetromino || Collisions.withBlocks tetromino blocks then
            updateX (tetromino.actualX + squareSize) tetromino

        else
            tetromino

    else
        tetromino


resolveRightCollision blocks keyboard tetromino =
    if keyboard.rightArrowPressed then
        if Collisions.rightScreen tetromino || Collisions.withBlocks tetromino blocks then
            updateX (tetromino.x - squareSize) tetromino

        else
            tetromino

    else
        tetromino


updateVerticalSpeed speed tetromino =
    { tetromino | verticalSpeed = speed }


stoppedMoving tetromino =
    tetromino.verticalSpeed == 0


generateRandomType msg =
    Random.generate msg randomTetrominoGenerator


randomTetrominoGenerator =
    Random.uniform I [ J, L, O, S, Z, T ]


updateBlocks tetromino =
    let
        blocks =
            BlockFactory.createByType
                tetromino.tetrominoType
                tetromino.rotation
                tetromino.x
                tetromino.y
    in
    { tetromino | blocks = blocks }

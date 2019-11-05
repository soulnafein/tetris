module Tetromino exposing
    ( Tetromino
    , generateNextBag
    , init
    , stoppedMoving
    , update
    )

import Block exposing (Block)
import BlockFactory
import Collisions
import Configuration
    exposing
        ( fallingSpeed
        , movingSpeed
        , squareSize
        )
import Keyboard exposing (Keyboard)
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


init : TetrominoType -> Tetromino
init tetrominoType =
    { x = 0
    , y = 0
    , actualX = 0
    , actualY = 0
    , tetrominoType = tetrominoType
    , verticalSpeed = toFloat fallingSpeed
    , rotation = North
    , hasJustRotated = False
    , blocks = []
    }
        |> updateBlocks


updateY : Float -> Tetromino -> Tetromino
updateY actualY tetromino =
    { tetromino | actualY = actualY, y = stickToGrid actualY }
        |> updateBlocks


updateX : Float -> Tetromino -> Tetromino
updateX actualX tetromino =
    { tetromino | actualX = actualX, x = stickToGrid actualX }
        |> updateBlocks


update : Float -> Keyboard -> Tetromino -> List Block -> Int -> Tetromino
update delta keyboard tetromino blocks score =
    tetromino
        |> updateFallingSpeed keyboard (toFloat score)
        |> fall delta
        |> resolveVerticalCollisions blocks
        |> horizontalMovement delta keyboard
        |> resolveHorizontalCollisions blocks keyboard
        |> updateRotation keyboard blocks


updateFallingSpeed : Keyboard -> Float -> Tetromino -> Tetromino
updateFallingSpeed keyboard score tetromino =
    let
        floatFallingSpeed =
            toFloat fallingSpeed

        modifiedSpeed =
            floatFallingSpeed * (1 + (score / 700))

        speed =
            if keyboard.bottomArrowPressed then
                modifiedSpeed + (5 * floatFallingSpeed)

            else
                modifiedSpeed
    in
    updateVerticalSpeed speed tetromino


updateRotation : Keyboard -> List Block -> Tetromino -> Tetromino
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
    if Collisions.horizontally blocks updatedTetromino.blocks then
        tetromino

    else
        updatedTetromino


fall : Float -> Tetromino -> Tetromino
fall delta tetromino =
    tetromino
        |> updateY (tetromino.actualY + (tetromino.verticalSpeed * delta))


stickToGrid : Float -> Float
stickToGrid aNumber =
    toFloat (floor (aNumber / toFloat squareSize) * squareSize)


horizontalMovement : Float -> Keyboard -> Tetromino -> Tetromino
horizontalMovement delta keyboard tetromino =
    let
        horizontalSpeed =
            calculateHorizontalSpeed keyboard

        updatedActualX =
            tetromino.actualX + (toFloat horizontalSpeed * delta)
    in
    tetromino
        |> updateX updatedActualX
        |> updateBlocks


calculateHorizontalSpeed : Keyboard -> Int
calculateHorizontalSpeed keyboard =
    if keyboard.leftArrowPressed then
        -movingSpeed

    else if keyboard.rightArrowPressed then
        movingSpeed

    else
        0


resolveVerticalCollisions : List Block -> Tetromino -> Tetromino
resolveVerticalCollisions blocks tetromino =
    let
        tetrominoBlocks =
            tetromino.blocks
    in
    if Collisions.bottomScreen tetrominoBlocks || Collisions.withBlocks tetrominoBlocks blocks then
        tetromino
            |> updateY (tetromino.y - toFloat squareSize)
            |> updateVerticalSpeed 0

    else
        tetromino


resolveHorizontalCollisions : List Block -> Keyboard -> Tetromino -> Tetromino
resolveHorizontalCollisions blocks keyboard tetromino =
    tetromino
        |> resolveLeftCollision blocks keyboard
        |> resolveRightCollision blocks keyboard


resolveLeftCollision : List Block -> Keyboard -> Tetromino -> Tetromino
resolveLeftCollision blocks keyboard tetromino =
    let
        tetrominoBlocks =
            tetromino.blocks
    in
    if keyboard.leftArrowPressed then
        if Collisions.leftScreen tetrominoBlocks || Collisions.withBlocks tetrominoBlocks blocks then
            updateX (tetromino.actualX + toFloat squareSize) tetromino

        else
            tetromino

    else
        tetromino


resolveRightCollision : List Block -> Keyboard -> Tetromino -> Tetromino
resolveRightCollision blocks keyboard tetromino =
    let
        tetrominoBlocks =
            tetromino.blocks
    in
    if keyboard.rightArrowPressed then
        if Collisions.rightScreen tetrominoBlocks || Collisions.withBlocks tetrominoBlocks blocks then
            updateX (tetromino.x - toFloat squareSize) tetromino

        else
            tetromino

    else
        tetromino


updateVerticalSpeed : Float -> Tetromino -> Tetromino
updateVerticalSpeed speed tetromino =
    { tetromino | verticalSpeed = speed }


stoppedMoving : Tetromino -> Bool
stoppedMoving tetromino =
    tetromino.verticalSpeed == 0


generateNextBag : Random.Seed -> ( List Tetromino, Random.Seed )
generateNextBag seed =
    Random.step tetrominoSequence seed


tetrominoSequence : Random.Generator (List Tetromino)
tetrominoSequence =
    Random.list 7 (Random.int 0 1000)
        |> Random.map
            (\list ->
                List.map2 Tuple.pair list [ I, J, O, L, S, Z, T ]
                    |> List.sortBy Tuple.first
                    |> List.map Tuple.second
                    |> List.map init
            )


updateBlocks : Tetromino -> Tetromino
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

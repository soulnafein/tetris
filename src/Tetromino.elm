module Tetromino exposing
    ( Tetromino
    , generateNextBag
    , init
    , stoppedMoving
    , update
    )

import Block exposing (Block)
import BlockMaker
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
    let
        blocksAndScreenFrame =
            blocks ++ BlockMaker.createScreenFrame
    in
    tetromino
        |> updateFallingSpeed keyboard (toFloat score)
        |> fall delta
        |> resolveCollisions blocks [ onVerticalCollision ]
        |> horizontalMovement delta keyboard
        |> resolveCollisions blocks [ onLeftCollision keyboard, onRightCollision keyboard ]
        |> updateRotation keyboard blocksAndScreenFrame


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
    if Collisions.withBlocks updatedTetromino.blocks blocks then
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


resolveCollisions : List Block -> List (Tetromino -> Tetromino) -> Tetromino -> Tetromino
resolveCollisions blocks funcs tetromino =
    List.foldl (resolveCollision (blocks ++ BlockMaker.createScreenFrame)) tetromino funcs


resolveCollision : List Block -> (Tetromino -> Tetromino) -> Tetromino -> Tetromino
resolveCollision blocks func tetromino =
    if Collisions.withBlocks tetromino.blocks blocks then
        func tetromino

    else
        tetromino


onVerticalCollision : Tetromino -> Tetromino
onVerticalCollision t =
    t
        |> updateY (t.y - toFloat squareSize)
        |> updateVerticalSpeed 0


onLeftCollision : Keyboard -> Tetromino -> Tetromino
onLeftCollision keyboard t =
    if keyboard.leftArrowPressed then
        updateX (t.actualX + toFloat squareSize) t

    else
        t


onRightCollision : Keyboard -> Tetromino -> Tetromino
onRightCollision keyboard t =
    if keyboard.rightArrowPressed then
        updateX (t.actualX - toFloat squareSize) t

    else
        t


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
            BlockMaker.createByType
                tetromino.tetrominoType
                tetromino.rotation
                tetromino.x
                tetromino.y
    in
    { tetromino | blocks = blocks }

module Collisions exposing
    ( bottomScreen
    , horizontally
    , leftScreen
    , rightScreen
    , withBlocks
    )

import Block exposing (Block)
import Configuration exposing (backgroundHeight, backgroundWidth, squareSize)


bottomScreen : List Block -> Bool
bottomScreen tetrominoBlocks =
    areCollidingWithHorizontalLine (toFloat backgroundHeight) tetrominoBlocks


leftScreen : List Block -> Bool
leftScreen tetrominoBlocks =
    areCollidingWithLeftLine 0 tetrominoBlocks


rightScreen : List Block -> Bool
rightScreen tetrominoBlocks =
    areCollidingWithRightLine (toFloat backgroundWidth) tetrominoBlocks


horizontally : List Block -> List Block -> Bool
horizontally blocks tetrominoBlocks =
    leftScreen tetrominoBlocks
        || rightScreen tetrominoBlocks
        || areCollidingTwoLists tetrominoBlocks blocks


withBlocks : List Block -> List Block -> Bool
withBlocks tetrominoBlocks blocks =
    areCollidingTwoLists tetrominoBlocks blocks


areColliding : Block -> Block -> Bool
areColliding a b =
    a.x == b.x && a.y == b.y


areCollidingOneBlockAndList : List Block -> Block -> Bool
areCollidingOneBlockAndList blocks a =
    blocks
        |> List.any (areColliding a)


areCollidingTwoLists : List Block -> List Block -> Bool
areCollidingTwoLists blocksA blocksB =
    blocksA
        |> List.any (areCollidingOneBlockAndList blocksB)


areCollidingWithHorizontalLine : Float -> List Block -> Bool
areCollidingWithHorizontalLine y blocks =
    blocks
        |> List.any (\b -> b.y + toFloat squareSize > y)


areCollidingWithLeftLine : Float -> List Block -> Bool
areCollidingWithLeftLine x blocks =
    blocks
        |> List.any (\b -> b.x < x)


areCollidingWithRightLine : Float -> List Block -> Bool
areCollidingWithRightLine x blocks =
    blocks
        |> List.any (\b -> (b.x + toFloat squareSize) > x)

module Collisions exposing (withBlocks)

import Block exposing (Block)
import Configuration exposing (backgroundHeight, backgroundWidth, squareSize)


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

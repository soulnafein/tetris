module Collisions exposing
    ( bottomScreen
    , horizontally
    , leftScreen
    , rightScreen
    , withBlocks
    )

import Block
import Configuration exposing (backgroundHeight, backgroundWidth, squareSize)


bottomScreen tetromino =
    areCollidingWithHorizontalLine backgroundHeight tetromino.blocks


leftScreen tetromino =
    areCollidingWithLeftLine 0 tetromino.blocks


rightScreen tetromino =
    areCollidingWithRightLine backgroundWidth tetromino.blocks


horizontally blocks tetromino =
    leftScreen tetromino
        || rightScreen tetromino
        || areCollidingTwoLists tetromino.blocks blocks


withBlocks tetromino blocks =
    areCollidingTwoLists tetromino.blocks blocks


areColliding a b =
    a.x == b.x && a.y == b.y


areCollidingOneBlockAndList blocks a =
    blocks
        |> List.any (areColliding a)


areCollidingTwoLists blocksA blocksB =
    blocksA
        |> List.any (areCollidingOneBlockAndList blocksB)


areCollidingWithHorizontalLine y blocks =
    blocks
        |> List.any (\b -> b.y + squareSize > y)


areCollidingWithLeftLine x blocks =
    blocks
        |> List.any (\b -> b.x < x)


areCollidingWithRightLine x blocks =
    blocks
        |> List.any (\b -> (b.x + squareSize) > x)

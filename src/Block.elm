module Block exposing
    ( Block
    , areCollidingTwoLists
    , areCollidingWithHorizontalLine
    , areCollidingWithLeftLine
    , areCollidingWithRightLine
    , createByType
    , update
    )

import Configuration exposing (gridWidth, squareSize)
import List exposing (..)
import Palette exposing (..)
import Rotation exposing (Rotation(..))
import TetrominoType exposing (TetrominoType(..))


type alias Block =
    { x : Float
    , y : Float
    , color : String
    }


verticalOverlapTwoBlocks a b =
    if areColliding a b then
        squareSize

    else
        0


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


createByType tetrominoType rotation x y =
    let
        blocksFunction =
            case tetrominoType of
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

        color =
            tetrominoColor tetrominoType
    in
    blocksFunction rotation
        |> createFromStrings color
        |> List.map (convertCoordinates x y)


createFromStrings color rows =
    List.indexedMap (stringToBlocks color) rows
        |> List.concat


stringToBlocks color y string =
    string
        |> String.indexes "#"
        |> List.map (\x -> create { x = toFloat x, y = toFloat y, color = color })


create block =
    { x = block.x
    , y = block.y
    , color = block.color
    }


tetrominoColor tetrominoType =
    case tetrominoType of
        I ->
            cyan

        J ->
            blue

        L ->
            orange

        O ->
            yellow

        S ->
            green

        Z ->
            red

        T ->
            purple


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
        North ->
            [ "O#OO"
            , "###O"
            ]

        East ->
            [ "O#OO"
            , "O##O"
            , "O#OO"
            ]

        South ->
            [ "OOOO"
            , "###O"
            , "O#OO"
            ]

        West ->
            [ "O#OO"
            , "##OO"
            , "O#OO"
            ]


convertCoordinates x y block =
    { block
        | x = block.x * squareSize + x
        , y = block.y * squareSize + y
    }


update ( blocks, points ) =
    let
        lineToDelete =
            nextLineToDelete blocks
    in
    case lineToDelete of
        Nothing ->
            ( blocks, points )

        Just y ->
            update ( removeLine y blocks, 50 + points )


nextLineToDelete blocks =
    let
        lines =
            blocks |> List.map .y
    in
    lines
        |> List.filter (lineFull blocks)
        |> List.head


lineFull blocks y =
    let
        numberOfBlocksInLine =
            blocks
                |> List.filter (\b -> b.y == y)
                |> List.length
    in
    numberOfBlocksInLine == gridWidth


removeLine y blocks =
    let
        blocksToMove =
            blocks
                |> List.filter (\b -> b.y < y)
                |> List.map (\b -> { b | y = b.y + squareSize })

        blocksToLeave =
            blocks
                |> List.filter (\b -> b.y > y)
    in
    blocksToMove ++ blocksToLeave

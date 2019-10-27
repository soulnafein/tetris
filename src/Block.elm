module Block exposing
    ( Block
    , areCollidingTwoLists
    , areCollidingWithHorizontalLine
    , areCollidingWithLeftLine
    , areCollidingWithRightLine
    , createByType
    )

import Configuration exposing (squareSize)
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
        |> List.map (\x -> { x = toFloat x, y = toFloat y, color = color })


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

module BlockMaker exposing (createByType, createScreenFrame)

import Block exposing (Block)
import Configuration exposing (gridHeight, gridWidth, squareSize)
import Palette
import Rotation exposing (Rotation(..))
import TetrominoType exposing (TetrominoType(..))


createByType : TetrominoType -> Rotation -> Float -> Float -> List Block
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


createFromStrings : String -> List String -> List Block
createFromStrings color rows =
    List.indexedMap (stringToBlocks color) rows
        |> List.concat


stringToBlocks : String -> Int -> String -> List Block
stringToBlocks color y string =
    string
        |> String.indexes "#"
        |> List.map (\x -> Block.create { x = toFloat x, y = toFloat y, color = color })


tetrominoColor : TetrominoType -> String
tetrominoColor tetrominoType =
    case tetrominoType of
        I ->
            Palette.cyan

        J ->
            Palette.blue

        L ->
            Palette.orange

        O ->
            Palette.yellow

        S ->
            Palette.green

        Z ->
            Palette.red

        T ->
            Palette.purple


blocksI : Rotation -> List String
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


blocksJ : Rotation -> List String
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


blocksL : Rotation -> List String
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


blocksO : Rotation -> List String
blocksO rotation =
    case rotation of
        _ ->
            [ "0##0"
            , "0##0"
            ]


blocksS : Rotation -> List String
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


blocksZ : Rotation -> List String
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


blocksT : Rotation -> List String
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


convertCoordinates : Float -> Float -> Block -> Block
convertCoordinates x y block =
    { block
        | x = block.x * toFloat squareSize + x
        , y = block.y * toFloat squareSize + y
    }


createScreenFrame : List Block
createScreenFrame =
    let
        x =
            [ String.repeat (gridWidth + 2) "#" ]

        body =
            "#" ++ String.repeat gridWidth " " ++ "#"

        b =
            List.repeat gridHeight body

        strings =
            x ++ b ++ x
    in
    createFromStrings "invisible" strings
        |> List.map (convertCoordinates (toFloat -squareSize) (toFloat -squareSize))

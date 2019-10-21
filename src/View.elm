module View exposing (render)

import Configuration exposing (squareSize)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Maybe
import Palette exposing (..)
import Svg exposing (..)
import Svg.Attributes as A
import Tetromino exposing (Tetromino, TetrominoType(..))
import Types exposing (Model, Msg)


render model =
    let
        currentTetromino =
            model.currentTetromino
    in
    div []
        [ svg
            [ A.width "300"
            , A.height "600"
            ]
            ([ drawBackground ]
                ++ renderTetrominos (model.tetrominos ++ [ currentTetromino ])
            )
        , Html.text (Maybe.withDefault "NO KEY PRESSED" model.keyboard.keyPressed)
        ]


drawBackground =
    rect [ A.width (String.fromInt (squareSize * 10)), A.height (String.fromInt (squareSize * 20)), A.fill grey ] []


renderTetrominos tetrominos =
    tetrominos
        |> List.concatMap renderTetromino


renderTetromino tetromino =
    let
        renderFunction =
            case tetromino.tetrominoType of
                I ->
                    renderI

                J ->
                    renderJ

                O ->
                    renderO

                S ->
                    renderS

                Z ->
                    renderZ

                T ->
                    renderT

                L ->
                    renderL
    in
    renderFunction tetromino.x tetromino.y


renderI x y =
    [ 0, 1, 2, 3 ]
        |> List.map (\i -> square (x + (squareSize * i)) y cyan)


renderJ x y =
    ([ 0, 1, 2 ]
        |> List.map (\i -> square (x + (squareSize * i)) (y + squareSize) blue)
    )
        ++ [ square x y blue ]


renderL x y =
    ([ 0, 1, 2 ]
        |> List.map (\i -> square (x + (squareSize * i)) (y + squareSize) orange)
    )
        ++ [ square (x + (squareSize * 2)) y orange ]


renderO x y =
    [ square x y yellow
    , square x (y + squareSize) yellow
    , square (x + squareSize) y yellow
    , square (x + squareSize) (y + squareSize) yellow
    ]


renderS x y =
    [ square x (y + squareSize) green
    , square (x + squareSize) (y + squareSize) green
    , square (x + squareSize) y green
    , square (x + squareSize) (y + (2 * squareSize)) green
    ]


renderZ x y =
    [ square x y red
    , square (x + squareSize) y red
    , square (x + squareSize) (y + squareSize) red
    , square (x + (2 * squareSize)) (y + squareSize) red
    ]


renderT x y =
    ([ 0, 1, 2 ]
        |> List.map (\i -> square (x + (squareSize * i)) (y + squareSize) purple)
    )
        ++ [ square (x + squareSize) y purple ]


square x y color =
    let
        size =
            String.fromInt squareSize

        roundedX =
            String.fromInt (round x)

        roundedY =
            String.fromInt (round y)
    in
    rect [ A.width size, A.height size, A.fill color, A.x roundedX, A.y roundedY ] []

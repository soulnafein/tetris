module View exposing (render)

import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Maybe
import Palette exposing (..)
import Svg exposing (..)
import Svg.Attributes as A
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
                ++ tetronimoI (round currentTetromino.x) (round currentTetromino.y)
                ++ tetronimoJ 100 120
                ++ tetronimoL 10 300
                ++ tetronimoO 50 500
                ++ tetronimoS 90 350
                ++ tetronimoZ 120 250
                ++ tetronimoT 160 400
            )
        , Html.text (Maybe.withDefault "NO KEY PRESSED" model.keyPressed)
        ]


drawBackground =
    rect [ A.width "300", A.height "600", A.fill grey ] []


tetronimoI x y =
    [ 0, 1, 2, 3 ]
        |> List.map (\i -> square (x + (squareWidth * i)) y cyan)


tetronimoJ x y =
    ([ 0, 1, 2 ]
        |> List.map (\i -> square (x + (squareWidth * i)) (y + squareWidth) blue)
    )
        ++ [ square x y blue ]


tetronimoL x y =
    ([ 0, 1, 2 ]
        |> List.map (\i -> square (x + (squareWidth * i)) (y + squareWidth) orange)
    )
        ++ [ square (x + (squareWidth * 2)) y orange ]


tetronimoO x y =
    [ square x y yellow
    , square x (y + squareWidth) yellow
    , square (x + squareWidth) y yellow
    , square (x + squareWidth) (y + squareWidth) yellow
    ]


tetronimoS x y =
    [ square x (y + squareWidth) green
    , square (x + squareWidth) (y + squareWidth) green
    , square (x + squareWidth) y green
    , square (x + squareWidth) (y + (2 * squareWidth)) green
    ]


tetronimoZ x y =
    [ square x y red
    , square (x + squareWidth) y red
    , square (x + squareWidth) (y + squareWidth) red
    , square (x + (2 * squareWidth)) (y + squareWidth) red
    ]


tetronimoT x y =
    ([ 0, 1, 2 ]
        |> List.map (\i -> square (x + (squareWidth * i)) (y + squareWidth) purple)
    )
        ++ [ square (x + squareWidth) y purple ]


squareWidth =
    30


square x y color =
    let
        size =
            String.fromInt squareWidth
    in
    rect [ A.width size, A.height size, A.fill color, A.x (String.fromInt x), A.y (String.fromInt y) ] []

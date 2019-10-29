module View exposing (render)

import Configuration exposing (backgroundHeight, backgroundWidth, squareSize)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Maybe
import Model exposing (Model, Msg)
import Palette exposing (..)
import Svg exposing (..)
import Svg.Attributes as A
import Tetromino exposing (Tetromino)


render model =
    let
        tetromino =
            model.tetromino
    in
    div []
        [ svg
            [ A.width (String.fromInt backgroundWidth)
            , A.height (String.fromInt backgroundHeight)
            ]
            ([ drawBackground ]
                ++ renderBlocks (model.blocks ++ tetromino.blocks)
            )
        , Html.text ("Score: " ++ String.fromInt model.score)
        ]


drawBackground =
    rect [ A.width (String.fromInt backgroundWidth), A.height (String.fromInt backgroundHeight), A.fill grey ] []


renderBlocks blocks =
    blocks
        |> List.map renderBlock


renderBlock block =
    let
        size =
            String.fromInt squareSize

        roundedX =
            String.fromInt (round block.x)

        roundedY =
            String.fromInt (round block.y)
    in
    rect [ A.width size, A.height size, A.fill block.color, A.x roundedX, A.y roundedY, A.stroke "black", A.strokeOpacity "0.2" ] []

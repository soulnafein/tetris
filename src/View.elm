module View exposing (render)

import Block exposing (Block)
import Configuration exposing (backgroundHeight, backgroundWidth, squareSize)
import Html exposing (Html, div, text)
import Model exposing (Model, Msg)
import Palette exposing (grey)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes as A
import Tetromino


render : Model -> Html Msg
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


drawBackground : Svg a
drawBackground =
    rect [ A.width (String.fromInt backgroundWidth), A.height (String.fromInt backgroundHeight), A.fill grey ] []


renderBlocks : List Block -> List (Svg a)
renderBlocks blocks =
    blocks
        |> List.map renderBlock


renderBlock : Block -> Svg a
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

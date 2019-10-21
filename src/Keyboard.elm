module Keyboard exposing (Keyboard, KeyboardAction(..), update)


type alias Keyboard =
    { leftArrowPressed : Bool
    , rightArrowPressed : Bool
    , keyPressed : Maybe String
    }


type KeyboardAction
    = Pressed
    | Released


update code keyboard action =
    let
        isPressed =
            case action of
                Pressed ->
                    True

                Released ->
                    False
    in
    keyboard
        |> updateKeyPressed code isPressed
        |> updateArrowState code isPressed


updateKeyPressed code isPressed keyboard =
    let
        keyPressed =
            if isPressed then
                Just code

            else
                Nothing
    in
    { keyboard | keyPressed = keyPressed }


updateArrowState code isPressed keyboard =
    case code of
        "ArrowLeft" ->
            { keyboard | leftArrowPressed = isPressed }

        "ArrowRight" ->
            { keyboard | rightArrowPressed = isPressed }

        _ ->
            keyboard

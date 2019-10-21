module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Configuration
    exposing
        ( fallingSpeed
        , movingSpeed
        , squareSize
        )
import Json.Decode as Decode
import Types exposing (Model, Msg(..))
import View


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            ( { model | keyPressed = Just code, keyboard = updateKeyboardDown code model.keyboard }, Cmd.none )

        KeyUp code ->
            ( { model | keyPressed = Nothing, keyboard = updateKeyboardUp code model.keyboard }, Cmd.none )

        FrameUpdate delta ->
            let
                deltaInSeconds =
                    delta / 1000
            in
            ( { model | currentTetromino = updateTetromino deltaInSeconds model.keyboard model.currentTetromino }, Cmd.none )

        _ ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( { keyPressed = Nothing
      , currentTetromino = { x = 0, y = 0, actualX = 0 }
      , keyboard = { leftArrowPressed = False, rightArrowPressed = False }
      }
    , Cmd.none
    )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = View.render
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta FrameUpdate
        , onKeyDown (Decode.map KeyDown keyDecoder)
        , onKeyUp (Decode.map KeyUp keyDecoder)
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


updateTetromino delta keyboard tetromino =
    let
        horizontalSpeed =
            if keyboard.leftArrowPressed then
                -movingSpeed

            else if keyboard.rightArrowPressed then
                movingSpeed

            else
                0

        updatedY =
            tetromino.y + (fallingSpeed * delta)

        updatedActualX =
            tetromino.actualX + (horizontalSpeed * delta)

        updatedX =
            toFloat (floor (updatedActualX / squareSize) * squareSize)
    in
    { tetromino | y = updatedY, x = updatedX, actualX = updatedActualX }


updateKeyboardDown code keyboard =
    case code of
        "ArrowLeft" ->
            { keyboard | leftArrowPressed = True }

        "ArrowRight" ->
            { keyboard | rightArrowPressed = True }

        _ ->
            keyboard


updateKeyboardUp code keyboard =
    case code of
        "ArrowLeft" ->
            { keyboard | leftArrowPressed = False }

        "ArrowRight" ->
            { keyboard | rightArrowPressed = False }

        _ ->
            keyboard

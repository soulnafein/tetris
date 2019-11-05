module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Json.Decode as Decode
import Keyboard
import Model exposing (Model, Msg(..))
import Random
import Tetromino
import Updates
import View


main : Program () Model Msg
main =
    Browser.element
        { view = View.render
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            Updates.onKeyChange model code Keyboard.Pressed

        KeyUp code ->
            Updates.onKeyChange model code Keyboard.Released

        FrameUpdate deltaMs ->
            Updates.onFrameUpdate model (deltaMs / 1000)

        InitialSeed initialSeed ->
            ( Model.init (Random.initialSeed initialSeed), Cmd.none )

        _ ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( Model.init (Random.initialSeed 0)
    , Random.generate InitialSeed (Random.int 0 9999999)
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta FrameUpdate
        , onKeyDown (Decode.map KeyDown keyDecoder)
        , onKeyUp (Decode.map KeyUp keyDecoder)
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string

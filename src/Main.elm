module Main exposing (..)

import Browser
import Types exposing (Model, Msg)
import View


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = View.render
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

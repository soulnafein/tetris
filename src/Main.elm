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
import Keyboard exposing (Keyboard)
import Tetromino exposing (Tetromino, TetrominoType(..))
import Types exposing (Model, Msg(..))
import View


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            ( { model | keyboard = Keyboard.update code model.keyboard Keyboard.Pressed }, Cmd.none )

        KeyUp code ->
            ( { model | keyboard = Keyboard.update code model.keyboard Keyboard.Released }, Cmd.none )

        FrameUpdate delta ->
            let
                deltaInSeconds =
                    delta / 1000
            in
            ( { model | currentTetromino = Tetromino.update deltaInSeconds model.keyboard model.currentTetromino }, Cmd.none )

        _ ->
            ( model, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( { currentTetromino = Tetromino.create { x = 0, y = 0, tetrominoType = I }
      , tetrominos = createTetrominos
      , keyboard = { keyPressed = Nothing, leftArrowPressed = False, rightArrowPressed = False }
      }
    , Cmd.none
    )


createTetrominos =
    Tetromino.createList
        [ { x = 100, y = 120, tetrominoType = J }
        , { x = 10, y = 30, tetrominoType = L }
        , { x = 50, y = 500, tetrominoType = O }
        , { x = 90, y = 350, tetrominoType = S }
        , { x = 120, y = 250, tetrominoType = Z }
        , { x = 160, y = 400, tetrominoType = T }
        ]



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

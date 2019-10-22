module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Json.Decode as Decode
import Keyboard exposing (Keyboard)
import Tetromino exposing (Tetromino, TetrominoType(..))
import Types exposing (Model, Msg(..))
import View


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            onKeyChange model code Keyboard.Pressed

        KeyUp code ->
            onKeyChange model code Keyboard.Released

        FrameUpdate delta ->
            onFrameUpdate model delta

        TetrominoGenerated tetrominoType ->
            onTetrominoGenerated model tetrominoType

        _ ->
            ( model, Cmd.none )


onFrameUpdate model delta =
    let
        deltaInSeconds =
            delta / 1000

        currentTetromino =
            Tetromino.update deltaInSeconds model.keyboard model.currentTetromino

        commands =
            if Tetromino.reachedBottom currentTetromino then
                [ Tetromino.generateRandomType TetrominoGenerated ]

            else
                []
    in
    ( { model | currentTetromino = currentTetromino }, Cmd.batch commands )


onKeyChange model code action =
    ( { model | keyboard = Keyboard.update code model.keyboard action }, Cmd.none )


onTetrominoGenerated model tetrominoType =
    let
        newTetromino =
            Tetromino.create { x = 0, y = 0, tetrominoType = tetrominoType }
    in
    ( { model | currentTetromino = newTetromino }, Cmd.none )


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

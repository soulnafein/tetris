module Main exposing (..)

import Block exposing (Block)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Json.Decode as Decode
import Keyboard exposing (Keyboard)
import Tetromino exposing (Tetromino)
import TetrominoType exposing (TetrominoType)
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

        updatedModel =
            updateModel deltaInSeconds model

        commands =
            if Tetromino.stoppedMoving updatedModel.currentTetromino then
                [ Tetromino.generateRandomType TetrominoGenerated ]

            else
                []
    in
    ( updatedModel, Cmd.batch commands )


simulationStep =
    0.02


updateModel delta model =
    if delta <= simulationStep then
        let
            currentTetromino =
                Tetromino.update delta model.keyboard model.currentTetromino model.blocks

            blocks =
                if Tetromino.stoppedMoving currentTetromino then
                    model.blocks ++ currentTetromino.blocks

                else
                    model.blocks

            ( updatedBlocks, points ) =
                Block.update ( blocks, 0 )
        in
        { model | currentTetromino = currentTetromino, blocks = updatedBlocks, score = model.score + points }

    else
        updateModel (delta - simulationStep) (updateModel simulationStep model)


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
    ( { currentTetromino = Tetromino.create { x = 0, y = 0, tetrominoType = TetrominoType.I }
      , blocks = []
      , keyboard = Keyboard.init
      , score = 0
      }
    , Tetromino.generateRandomType TetrominoGenerated
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

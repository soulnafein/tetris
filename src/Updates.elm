module Updates exposing (onFrameUpdate, onKeyChange, onTetrominoGenerated)

import Block
import Keyboard exposing (KeyboardAction)
import Model exposing (Model, Msg(..))
import Tetromino
import TetrominoType exposing (TetrominoType)


onKeyChange : Model -> String -> KeyboardAction -> ( Model, Cmd Msg )
onKeyChange model code action =
    ( { model | keyboard = Keyboard.update code model.keyboard action }
    , Cmd.none
    )


onTetrominoGenerated : Model -> TetrominoType -> ( Model, Cmd Msg )
onTetrominoGenerated model tetrominoType =
    ( { model | tetromino = Tetromino.init tetrominoType }
    , Cmd.none
    )


onFrameUpdate : Model -> Float -> ( Model, Cmd Msg )
onFrameUpdate model delta =
    let
        updatedModel =
            Model.update delta model

        commands =
            if Tetromino.stoppedMoving updatedModel.tetromino then
                [ Tetromino.generateRandomType TetrominoGenerated ]

            else
                []
    in
    if updatedModel.tetromino.y < 0 then
        ( Model.init
        , Tetromino.generateRandomType TetrominoGenerated
        )

    else
        ( updatedModel, Cmd.batch commands )

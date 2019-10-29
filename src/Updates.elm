module Updates exposing (onFrameUpdate, onKeyChange, onTetrominoGenerated)

import Block exposing (Block)
import Keyboard exposing (Keyboard)
import Model exposing (Model, Msg(..))
import Tetromino exposing (Tetromino)


onKeyChange model code action =
    ( { model | keyboard = Keyboard.update code model.keyboard action }
    , Cmd.none
    )


onTetrominoGenerated model tetrominoType =
    ( { model | tetromino = Tetromino.init tetrominoType }
    , Cmd.none
    )


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

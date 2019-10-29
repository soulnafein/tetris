module Model exposing (Model, Msg(..), init, update)

import Block exposing (Block)
import Keyboard exposing (Keyboard)
import Tetromino exposing (Tetromino)
import TetrominoType exposing (TetrominoType)


type alias Model =
    { tetromino : Tetromino
    , blocks : List Block
    , keyboard : Keyboard
    , score : Int
    }


type Msg
    = NoOp
    | FrameUpdate Float
    | KeyUp String
    | KeyDown String
    | TetrominoGenerated TetrominoType


init =
    { tetromino = Tetromino.init TetrominoType.I
    , blocks = []
    , keyboard = Keyboard.init
    , score = 0
    }


update delta model =
    if delta <= simulationStep then
        updateOneStep delta model

    else if delta > simulationStep * 20 then
        model

    else
        update (delta - simulationStep) (updateOneStep simulationStep model)


updateOneStep delta model =
    let
        tetromino =
            Tetromino.update delta model.keyboard model.tetromino model.blocks model.score

        blocks =
            if Tetromino.stoppedMoving tetromino then
                model.blocks ++ tetromino.blocks

            else
                model.blocks

        ( updatedBlocks, points ) =
            Block.update ( blocks, 0 )
    in
    { model | tetromino = tetromino, blocks = updatedBlocks, score = model.score + points }


simulationStep =
    0.02

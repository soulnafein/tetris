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
    , seed : Int
    , nextTetrominos : List Tetromino
    }


type Msg
    = NoOp
    | FrameUpdate Float
    | KeyUp String
    | KeyDown String
    | TetrominoGenerated TetrominoType
    | InitialSeed Int


init : Model
init =
    { tetromino = Tetromino.init TetrominoType.I
    , blocks = []
    , keyboard = Keyboard.init
    , score = 0
    , seed = 0
    , nextTetrominos = []
    }


update : Float -> Model -> Model
update delta model =
    if delta <= simulationStep then
        updateOneStep delta model

    else if delta > simulationStep * 20 then
        model

    else
        update (delta - simulationStep) (updateOneStep simulationStep model)


updateOneStep : Float -> Model -> Model
updateOneStep delta model =
    model
        |> updateTetromino delta
        |> addTetrominoToBlocks
        |> checkRowCompletion


updateTetromino : Float -> Model -> Model
updateTetromino delta model =
    { model
        | tetromino = Tetromino.update delta model.keyboard model.tetromino model.blocks model.score
    }


addTetrominoToBlocks : Model -> Model
addTetrominoToBlocks model =
    if Tetromino.stoppedMoving model.tetromino then
        { model
            | blocks = model.blocks ++ model.tetromino.blocks
        }

    else
        model


checkRowCompletion : Model -> Model
checkRowCompletion model =
    let
        ( blocks, points ) =
            Block.update ( model.blocks, 0 )
    in
    { model
        | blocks = blocks
        , score = model.score + points
    }


simulationStep : Float
simulationStep =
    0.02

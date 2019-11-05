module Model exposing (Model, Msg(..), init, update)

import Block exposing (Block)
import Keyboard exposing (Keyboard)
import Random
import Tetromino exposing (Tetromino)
import TetrominoType exposing (TetrominoType)


type alias Model =
    { tetromino : Tetromino
    , blocks : List Block
    , keyboard : Keyboard
    , score : Int
    , seed : Random.Seed
    , nextTetrominos : List Tetromino
    }


type Msg
    = NoOp
    | FrameUpdate Float
    | KeyUp String
    | KeyDown String
    | InitialSeed Int


init : Random.Seed -> Model
init seed =
    { tetromino = Tetromino.init TetrominoType.I
    , blocks = []
    , keyboard = Keyboard.init
    , score = 0
    , seed = seed
    , nextTetrominos = []
    }
        |> updateNextTetrominos
        |> pickNextTetromino


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
        |> updateNextTetrominos
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
    let
        tetrominoStoppedMoving =
            Tetromino.stoppedMoving model.tetromino

        tetrominoOutsideGrid =
            model.tetromino.y < 0
    in
    if tetrominoStoppedMoving then
        if tetrominoOutsideGrid then
            init model.seed

        else
            { model | blocks = model.blocks ++ model.tetromino.blocks }
                |> pickNextTetromino

    else
        model


pickNextTetromino : Model -> Model
pickNextTetromino model =
    { model
        | tetromino = List.head model.nextTetrominos |> Maybe.withDefault model.tetromino
        , nextTetrominos = List.tail model.nextTetrominos |> Maybe.withDefault []
    }


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


updateNextTetrominos : Model -> Model
updateNextTetrominos model =
    if nextTetrominosAlmostEmpty model then
        let
            ( nextTetrominos, newSeed ) =
                Tetromino.generateNextBag model.seed
        in
        { model
            | nextTetrominos =
                model.nextTetrominos ++ nextTetrominos
            , seed = newSeed
        }

    else
        model


nextTetrominosAlmostEmpty : Model -> Bool
nextTetrominosAlmostEmpty model =
    List.length model.nextTetrominos < 3


simulationStep : Float
simulationStep =
    0.02

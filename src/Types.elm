module Types exposing (Model, Msg(..))

import Block exposing (Block)
import Keyboard exposing (Keyboard)
import Tetromino exposing (Tetromino)
import TetrominoType exposing (TetrominoType)


type alias Model =
    { currentTetromino : Tetromino
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

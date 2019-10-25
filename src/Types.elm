module Types exposing (Model, Msg(..))

import Keyboard exposing (Keyboard)
import Tetromino exposing (Block, Tetromino, TetrominoType)


type alias Model =
    { currentTetromino : Tetromino
    , blocks : List Block
    , keyboard : Keyboard
    }


type Msg
    = NoOp
    | FrameUpdate Float
    | KeyUp String
    | KeyDown String
    | TetrominoGenerated TetrominoType

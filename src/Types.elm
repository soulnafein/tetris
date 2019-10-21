module Types exposing (Model, Msg(..))

import Keyboard exposing (Keyboard)
import Tetromino exposing (Tetromino)


type alias Model =
    { currentTetromino : Tetromino
    , tetrominos : List Tetromino
    , keyboard : Keyboard
    }


type Msg
    = NoOp
    | FrameUpdate Float
    | KeyUp String
    | KeyDown String

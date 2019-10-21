module Types exposing (Model, Msg(..))


type alias Model =
    { keyPressed : Maybe String
    , currentTetromino : Tetromino
    , keyboard : Keyboard
    }


type alias Tetromino =
    { x : Float
    , y : Float
    , actualX : Float
    }


type Msg
    = NoOp
    | FrameUpdate Float
    | KeyUp String
    | KeyDown String


type alias Keyboard =
    { leftArrowPressed : Bool, rightArrowPressed : Bool }

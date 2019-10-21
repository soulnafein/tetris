module Types exposing (Model, Msg(..), TetrominoType(..))


type alias Model =
    { keyPressed : Maybe String
    , currentTetromino : Tetromino
    , tetrominos : List Tetromino
    , keyboard : Keyboard
    }


type TetrominoType
    = I
    | J
    | L
    | O
    | S
    | Z
    | T


type alias Tetromino =
    { x : Float
    , y : Float
    , actualX : Float
    , tetrominoType : TetrominoType
    }


type Msg
    = NoOp
    | FrameUpdate Float
    | KeyUp String
    | KeyDown String


type alias Keyboard =
    { leftArrowPressed : Bool, rightArrowPressed : Bool }

module Configuration exposing
    ( backgroundHeight
    , backgroundWidth
    , fallingSpeed
    , gridHeight
    , gridWidth
    , movingSpeed
    , squareSize
    )


squareSize : Int
squareSize =
    30


fallingSpeed : Int
fallingSpeed =
    squareSize * 3


movingSpeed : Int
movingSpeed =
    squareSize * 10


gridHeight : Int
gridHeight =
    20


backgroundHeight : Int
backgroundHeight =
    squareSize * gridHeight


gridWidth : Int
gridWidth =
    10


backgroundWidth : Int
backgroundWidth =
    squareSize * gridWidth

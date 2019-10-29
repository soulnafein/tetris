module Configuration exposing
    ( backgroundHeight
    , backgroundWidth
    , fallingSpeed
    , gridHeight
    , gridWidth
    , movingSpeed
    , squareSize
    )


squareSize =
    30


fallingSpeed =
    squareSize * 3


movingSpeed =
    squareSize * 10


gridHeight =
    20


backgroundHeight =
    squareSize * gridHeight


gridWidth =
    10


backgroundWidth =
    squareSize * gridWidth

module Direction exposing (Direction(..), flip, fromKey, shiftClockwise)


type Direction
    = Up
    | Right
    | Down
    | Left


shiftClockwise : Direction -> Direction
shiftClockwise direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


flip : Direction -> Direction
flip direction =
    case direction of
        Up ->
            Down

        Right ->
            Left

        Down ->
            Up

        Left ->
            Right


fromKey : String -> Maybe Direction
fromKey key =
    case String.uncons key of
        Just ( 'w', "" ) ->
            Just Up

        Just ( 'd', "" ) ->
            Just Right

        Just ( 's', "" ) ->
            Just Down

        Just ( 'a', "" ) ->
            Just Left

        _ ->
            Nothing

module Direction exposing (Direction(..), Rotation(..), fromKey, reverse, rotate, toAngle)


type Direction
    = Up
    | Right
    | Down
    | Left


type Rotation
    = Clockwise
    | CounterClock


rotate : Rotation -> Direction -> Direction
rotate course direction =
    let
        clockwise =
            case direction of
                Up ->
                    Right

                Right ->
                    Down

                Down ->
                    Left

                Left ->
                    Up
    in
    case course of
        Clockwise ->
            clockwise

        CounterClock ->
            reverse clockwise


reverse : Direction -> Direction
reverse direction =
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


toAngle : Direction -> Float
toAngle direction =
    case direction of
        Down ->
            turns 0

        Left ->
            turns 0.25

        Up ->
            turns 0.5

        Right ->
            turns 0.75

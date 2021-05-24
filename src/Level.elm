module Level exposing (Code(..), Level, Outcome(..), first, list)


type alias Level =
    { name : String
    , description : String
    , size : Int
    , criteria : Criteria
    , tests : List (List Code)
    }


type alias Criteria =
    List Code -> Outcome -> Bool


type Code
    = Red
    | Blue


type Outcome
    = Passed (List Code)
    | Failed


first : Level
first =
    { name = "Starter Level"
    , description = "Allow all robots to get safely from the start to the end"
    , size = 3
    , criteria =
        \_ outcome ->
            case outcome of
                Passed _ ->
                    True

                Failed ->
                    False
    , tests =
        [ []
        , [ Red ]
        , [ Blue ]
        ]
    }


list : List Level
list =
    [ first
    , { name = "Checking ID"
      , description = "Allow only robots whose codes start with Red"
      , size = 4
      , criteria =
            \initial outcome ->
                case ( initial, outcome ) of
                    ( Red :: _, Passed _ ) ->
                        True

                    ( Red :: _, Failed ) ->
                        False

                    ( _, Passed _ ) ->
                        False

                    ( _, Failed ) ->
                        True
      , tests =
            [ []
            , [ Red, Red, Blue ]
            , [ Red, Blue, Red ]
            , [ Blue ]
            ]
      }
    , { name = "Empty Your Pockets"
      , description = "Clear all the colors from each robot before letting them through the end"
      , size = 4
      , criteria =
            \_ outcome ->
                case outcome of
                    Passed [] ->
                        True

                    _ ->
                        False
      , tests =
            [ []
            , [ Blue ]
            , [ Red, Blue, Red, Blue ]
            , List.concat <| List.repeat 10 [ Red, Blue ]
            ]
      }
    ]

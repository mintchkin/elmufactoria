module Level exposing (Code(..), Level, Outcome(..), first, list)


type alias Level =
    { name : String
    , description : String
    , size : Int
    , criteria : Criteria
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
    }


list : List Level
list =
    [ first
    , { name = "Checking ID"
      , description = "Allow only robots whose codes start with Red"
      , size = 4
      , criteria =
            \initial outcome ->
                case List.head initial of
                    Just Red ->
                        case outcome of
                            Passed _ ->
                                True

                            Failed ->
                                False

                    _ ->
                        case outcome of
                            Failed ->
                                True

                            Passed _ ->
                                False
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
      }
    ]

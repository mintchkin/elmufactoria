module Level exposing (Code(..), Level, Result(..), first, list)


type alias Level =
    { name : String
    , description : String
    , size : Int
    , criteria : Criteria
    }


type alias Criteria =
    List Code -> Result -> Bool


type Code
    = Red
    | Blue


type Result
    = Passed (List Code)
    | Failed


first : Level
first =
    { name = "Starter Level"
    , description = "Allow all robots to get safely from the start to the end"
    , size = 3
    , criteria =
        \_ result ->
            case result of
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
            \initial result ->
                case List.head initial of
                    Just Red ->
                        case result of
                            Passed _ ->
                                True

                            Failed ->
                                False

                    _ ->
                        case result of
                            Failed ->
                                True

                            Passed _ ->
                                False
      }
    , { name = "Empty Your Pockets"
      , description = "Clear all the colors from each robot before letting them through the end"
      , size = 4
      , criteria =
            \_ result ->
                case result of
                    Passed [] ->
                        True

                    _ ->
                        False
      }
    ]

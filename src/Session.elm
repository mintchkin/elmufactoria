port module Session exposing (Session, Store, fromStore, getGrid, getLevel, receive, request, save)

import Array exposing (Array)
import Json.Decode as D exposing (Decoder, Value)
import Json.Encode as E
import Level exposing (Level)
import Tile exposing (Tile)


type Session
    = Session Level (Array Tile)


type Store
    = Store Value


getLevel : Session -> Level
getLevel (Session level _) =
    level


getGrid : Session -> Array Tile
getGrid (Session _ grid) =
    grid


decodeProgress : Decoder ( String, Array Tile )
decodeProgress =
    D.map2 Tuple.pair
        (D.field "key" D.string)
        (D.field "data" <| D.array Tile.fromJson)



--- PORTS ---


port requestStorage : Value -> Cmd msg


port receiveStorage : (Value -> msg) -> Sub msg



--- API ---


save : Level -> Array Tile -> Cmd msg
save level grid =
    requestStorage <|
        E.object
            [ ( "op", E.string "Save" )
            , ( "key", E.string level.name )
            , ( "data", E.array Tile.toJson grid )
            ]


request : Level -> Cmd msg
request level =
    requestStorage <|
        E.object
            [ ( "op", E.string "Load" )
            , ( "key", E.string level.name )
            ]


receive : (Store -> msg) -> Sub msg
receive getMsg =
    receiveStorage (\v -> getMsg (Store v))


fromStore : Level -> Store -> Session
fromStore level (Store value) =
    case D.decodeValue decodeProgress value of
        Ok ( name, grid ) ->
            if name == level.name then
                Session level grid

            else
                Session level (Tile.initBoard level.size)

        Err _ ->
            Session level (Tile.initBoard level.size)

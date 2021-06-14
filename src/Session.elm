port module Session exposing (..)

import Array exposing (Array)
import Json.Decode as JsonD
import Json.Encode as JsonE
import Level exposing (Level)
import Tile exposing (Tile)


type Session
    = Session Level (Array Tile)



--- PORTS ---


port requestStorage : JsonE.Value -> Cmd msg


port receiveStorage : (JsonE.Value -> msg) -> Sub msg


saveProgress : Level -> Array Tile -> Cmd msg
saveProgress level grid =
    requestStorage <|
        JsonE.object
            [ ( "op", JsonE.string "Save" )
            , ( "key", JsonE.string level.name )
            , ( "data", JsonE.array Tile.toJson grid )
            ]


requestProgress : Level -> Cmd msg
requestProgress level =
    requestStorage <|
        JsonE.object
            [ ( "op", JsonE.string "Load" )
            , ( "key", JsonE.string level.name )
            ]


decodeProgress : JsonD.Decoder ( String, Array Tile )
decodeProgress =
    JsonD.map2 Tuple.pair
        (JsonD.field "key" JsonD.string)
        (JsonD.field "data" <| JsonD.array Tile.fromJson)

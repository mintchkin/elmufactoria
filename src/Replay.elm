module Replay exposing (..)

import Array exposing (Array)
import Element exposing (Element)
import Level exposing (Level)
import Robot exposing (Progress(..), Robot, advance)
import Tile exposing (Tile)
import Time


type alias Model =
    { grid : Array Tile
    , speed : Int
    , robots : List Robot
    }


init : Array Tile -> Level -> Model
init grid level =
    { grid = grid
    , speed = 1
    , robots = Robot.initAll level
    }



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.speed > 0 then
        Time.every (1000 / toFloat model.speed) (\_ -> StepReplay)

    else
        Sub.none



--- UPDATE ---


type Msg
    = PressKey String
    | StepReplay
    | IncreaseSpeed
    | DecreaseSpeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressKey _ ->
            ( model, Cmd.none )

        StepReplay ->
            case model.robots of
                [] ->
                    ( model, Cmd.none )

                robot :: rest ->
                    case advance model.grid robot of
                        Finished _ ->
                            ( { model | robots = rest }, Cmd.none )

                        Working bot ->
                            ( { model | robots = bot :: rest }, Cmd.none )

        IncreaseSpeed ->
            ( { model | speed = clamp 0 10 (model.speed + 1) }, Cmd.none )

        DecreaseSpeed ->
            ( { model | speed = clamp 0 10 (model.speed - 1) }, Cmd.none )



--- VIEW ---


viewLeftPanel : Model -> Element Msg
viewLeftPanel =
    Debug.todo "Implement left panel"


viewRightPanel : Model -> Element Msg
viewRightPanel =
    Debug.todo "Implement right panel"


viewBottomPanel : Model -> Element Msg
viewBottomPanel =
    Debug.todo "Implement bottom panel"
